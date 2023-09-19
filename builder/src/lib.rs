use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Attribute, Data, DeriveInput, Fields, GenericArgument, Ident, LitStr,
    PathArguments, Token, Type,
};

struct BuilderParser {
    ident: Ident,
    value: Ident,
}

impl Parse for BuilderParser {
    #[inline]
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token!(=)>()?;
        let value = input.parse::<LitStr>()?.value();
        let value = format_ident!("{}", value);

        Ok(BuilderParser { ident, value })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Prase the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let impl_fields = get_impl_fields(&input.data);
    let struct_fields = get_struct_fields(&input.data);
    let impl_funcs = get_impl_funcs(&input.data);
    let build_return = get_build_return(&input.data, &name);
    // Build the output, possibly using quasi-qutation
    let expanded = quote! {
        use std::error::Error;

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #impl_fields
                }
            }
        }

        pub struct #builder_name {
            #struct_fields
        }

        impl #builder_name {
            #impl_funcs
            #build_return
        }
    };
    let tokens = proc_macro::TokenStream::from(expanded);
    tokens
}

fn optional_type(ty: &Type) -> Option<Type> {
    let mut out_ty = None;
    if let Type::Path(ty_path) = ty {
        for seg in &ty_path.path.segments {
            if seg.ident == "Option" {
                if let PathArguments::AngleBracketed(ref ang_brack) = seg.arguments {
                    for arg in &ang_brack.args {
                        if let GenericArgument::Type(ty) = arg {
                            out_ty = Some(ty.clone());
                        }
                    }
                }
            }
        }
    }
    out_ty
}

fn attr_builder_value(attrs: &Vec<Attribute>) -> Option<BuilderParser> {
    for attr in attrs {
        if attr.path().is_ident("builder") {
            let builder: BuilderParser = attr
                .parse_args()
                .expect("failed to parse buidler attribute");
            return Some(builder);
        }
    }
    None
}

fn get_build_return(data: &Data, name: &Ident) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse_if = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    attr_builder_value(&f.attrs);
                    if let Some(_ty) = optional_type(&f.ty) {
                        quote_spanned! { f.span()=>
                            let #name = self.#name.clone();
                        }
                    } else {
                        quote_spanned! { f.span()=>
                            let Some(#name) = self.#name.clone() else { return Err("Not all fields are populated".into()) };
                        }
                    }
                });
                let recurse_ok = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span()=>
                        #name,
                    }
                });
                quote! {
                    pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                        #(#recurse_if)*
                        Ok(#name {
                            #(#recurse_ok)*
                        })
                    }
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn get_impl_funcs(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if let Some(builder) = attr_builder_value(&f.attrs) {
                        let val_name = builder.value;
                        if builder.ident == "each" {
                            if let Some(ty) = optional_type(&f.ty) {
                                quote_spanned! { f.span()=>
                                    fn #val_name(&mut self, #val_name: #ty) -> &mut Self {
                                        if let Some(#name) = self.#name {
                                            self.#name.push(#val_name);
                                        } else {
                                            self.#name = vec![#val_name];
                                        }
                                        self
                                    }
                                }
                            } else {
                                quote_spanned! { f.span()=>
                                    fn #val_name(&mut self, #val_name: #ty) -> &mut Self {
                                        if let Some(#name) = self.#name {
                                            self.#name.push(#val_name);
                                        } else {
                                            self.#name = vec![#val_name];
                                        }
                                        self
                                    }
                                }
                            }
                        } else {
                            if let Some(ty) = optional_type(&f.ty) {
                                quote_spanned! { f.span()=>
                                    fn #name(&mut self, #name: #ty) -> &mut Self {
                                        self.#name = Some(#name);
                                        self
                                    }
                                }
                            } else {
                                quote_spanned! { f.span()=>
                                    fn #name(&mut self, #name: #ty) -> &mut Self {
                                        self.#name = Some(#name);
                                        self
                                    }
                                }
                            }
                        }
                    } else {
                        if let Some(ty) = optional_type(&f.ty) {
                            quote_spanned! { f.span()=>
                                fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = Some(#name);
                                    self
                                }
                            }
                        } else {
                            quote_spanned! { f.span()=>
                                fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = Some(#name);
                                    self
                                }
                            }
                        }
                    }
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn get_impl_fields(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span()=>
                        #name: None,
                    }
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn get_struct_fields(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if let Some(ty) = optional_type(&f.ty) {
                        quote_spanned! { f.span()=>
                            #name: Option<#ty>,
                        }
                    } else {
                        quote_spanned! { f.span()=>
                            #name: Option<#ty>,
                        }
                    }
                });
                quote! {
                    #(#recurse)*
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
