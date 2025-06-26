use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Variant};

/// The Agent derive macro
#[proc_macro_derive(Agent)]
pub fn agent_derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let struct_name = &input.ident;

    let code = quote::quote! {
        #[async_trait::async_trait]
        impl komrad_core::Agent for #struct_name {
            fn spawn(mut self) -> komrad_core::Channel {
                let (channel, listener) = komrad_core::Channel::new();

                let initializer_map = indexmap::IndexMap::new();

                let run_channel = channel.clone();
                tokio::spawn(async move {
                    self.run(run_channel, listener, initializer_map).await;
                });

                channel
            }

            fn spawn_with_initializer(mut self: Box<Self>, initializer_map: indexmap::IndexMap<String, komrad_core::Value>) -> komrad_core::Channel {
                let (channel, listener) = komrad_core::Channel::new();

                let run_channel = channel.clone();
                tokio::spawn(async move {
                    self.run(run_channel, listener, initializer_map).await;
                });

                channel
            }
        }
    };
    code.into()
}

/// The macro that provides TryFrom<Message> and etc for custom message types
///
/// ```
/// use komrad_core::Channel;
/// use komrad_macros::AgentMessage;
///
/// #[derive(AgentMessage)]
/// pub struct TestAgentMessage {
///     #[exact_string = "send"]
///     _command: String,
///     target: Channel,
/// }
/// ```
#[proc_macro_derive(AgentMessage, attributes(exact_string))]
pub fn agent_message_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    match &input.data {
        Data::Struct(data) => derive_for_struct(name, data),
        Data::Enum(data) => derive_for_enum(name, &data.variants),
        Data::Union(_) => panic!("AgentMessage cannot be derived for unions"),
    }
}

fn derive_for_struct(name: &syn::Ident, data: &syn::DataStruct) -> TokenStream {
    let fields = match &data.fields {
        Fields::Named(fields) => &fields.named,
        _ => panic!("AgentMessage only supports named fields for structs"),
    };

    let mut field_assignments = Vec::new();
    let mut field_reconstructs = Vec::new();
    let mut field_names = Vec::new();
    let mut index = 0usize;

    for field in fields.iter() {
        let field_name = field.ident.as_ref().unwrap();
        field_names.push(field_name);
        let ty = &field.ty;

        let mut exact_string = None;

        for attr in &field.attrs {
            if attr.path().is_ident("exact_string") {
                if let Ok(expr) = attr.parse_args::<syn::Expr>() {
                    match expr {
                        syn::Expr::Lit(lit_expr) => {
                            if let syn::Lit::Str(lit_str) = lit_expr.lit {
                                exact_string = Some(lit_str.value());
                            }
                        }
                        syn::Expr::Path(path_expr) => {
                            if let Some(ident) = path_expr.path.get_ident() {
                                exact_string = Some(ident.to_string());
                            }
                        }
                        _ => {}
                    }
                } else {
                    attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("") {
                            let lit: syn::LitStr = meta.value()?.parse()?;
                            exact_string = Some(lit.value());
                        }
                        Ok(())
                    })
                        .ok();
                }
            }
        }

        if let Some(expected) = exact_string {
            let expected_lit = syn::LitStr::new(&expected, proc_macro2::Span::call_site());

            field_assignments.push(quote! {
                let #field_name = {
                    match msg.value() {
                        komrad_core::Value::List(terms) => {
                            if terms.len() <= #index {
                                return Err(komrad_core::RuntimeError::ArgumentError(format!(
                                    "Message has too few terms, expected at least {} but got {}",
                                    #index + 1, terms.len()
                                )));
                            }

                            let value = &terms[#index];
                            match value {
                                komrad_core::Value::String(s) => {
                                    if s != #expected_lit {
                                        return Err(komrad_core::RuntimeError::ArgumentError(format!(
                                            "Expected string '{}' at position {}, found '{}'",
                                            #expected_lit, #index, s
                                        )));
                                    }
                                    #expected_lit.to_string()
                                },
                                komrad_core::Value::Word(s) => {
                                    if s != #expected_lit {
                                        return Err(komrad_core::RuntimeError::ArgumentError(format!(
                                            "Expected word '{}' at position {}, found '{}'",
                                            #expected_lit, #index, s
                                        )));
                                    }
                                    #expected_lit.to_string()
                                },
                                _ => return Err(komrad_core::RuntimeError::ArgumentError(format!(
                                    "Expected string or word '{}' at position {}, but got {:?}",
                                    #expected_lit, #index, value
                                ))),
                            }
                        },
                        _ => return Err(komrad_core::RuntimeError::ArgumentError(
                            "Message value must be a list of terms".to_string()
                        )),
                    }
                };
            });

            field_reconstructs.push(quote! {
                komrad_core::Value::String(self.#field_name.clone())
            });
        } else {
            field_assignments.push(quote! {
                let #field_name: #ty = {
                    match msg.value() {
                        komrad_core::Value::List(terms) => {
                            if terms.len() <= #index {
                                return Err(komrad_core::RuntimeError::ArgumentError(format!(
                                    "Message has too few terms, expected at least {} but got {}",
                                    #index + 1, terms.len()
                                )));
                            }

                            let value = &terms[#index];
                            <#ty>::try_from(value.clone()).map_err(|_|
                                komrad_core::RuntimeError::ArgumentError(format!(
                                    "Invalid value at position {} for field '{}': {:?}",
                                    #index, stringify!(#field_name), value
                                ))
                            )?
                        },
                        _ => return Err(komrad_core::RuntimeError::ArgumentError(
                            "Message value must be a list of terms".to_string()
                        )),
                    }
                };
            });

            field_reconstructs.push(quote! {
                komrad_core::Value::from(self.#field_name.clone())
            });
        }

        index += 1;
    }

    let expanded = quote! {
        impl TryFrom<&komrad_core::Message> for #name {
            type Error = komrad_core::RuntimeError;

            fn try_from(msg: &komrad_core::Message) -> Result<Self, Self::Error> {
                #(#field_assignments)*

                Ok(Self {
                    #(#field_names: #field_names),*
                })
            }
        }

        impl #name {
            pub fn to_message(&self) -> komrad_core::Message {
                komrad_core::Message::default().with_terms(vec![
                    #(#field_reconstructs),*
                ])
            }
        }
    };

    TokenStream::from(expanded)
}

fn derive_for_enum(
    name: &syn::Ident,
    variants: &syn::punctuated::Punctuated<Variant, syn::token::Comma>,
) -> TokenStream {
    let variant_cases = variants.iter().map(|variant| {
        let variant_name = &variant.ident;

        // Find the exact_string attribute
        let exact_string_attr = variant.attrs.iter().find(|attr| {
            attr.path().is_ident("exact_string")
        });

        // Parse the attribute value
        let expected_str = if let Some(attr) = exact_string_attr {
            match &attr.meta {
                syn::Meta::NameValue(nv) => {
                    // Handle #[exact_string = "value"] format
                    if let syn::Expr::Lit(lit) = &nv.value {
                        if let syn::Lit::Str(lit_str) = &lit.lit {
                            lit_str.value()
                        } else {
                            panic!("exact_string attribute must have a string literal value");
                        }
                    } else {
                        panic!("exact_string attribute must have a string literal value");
                    }
                }
                syn::Meta::List(list) => {
                    // Handle #[exact_string(value)] format
                    let tokens = list.tokens.clone();
                    let token_stream: proc_macro::TokenStream = tokens.into();
                    let token_string = token_stream.to_string();
                    token_string.trim().trim_matches('"').to_string()
                }
                _ => panic!("exact_string attribute must be either #[exact_string = \"value\"] or #[exact_string(value)]"),
            }
        } else {
            panic!("AgentMessage enum variants must have an #[exact_string] attribute");
        };

        // Check variant fields
        match &variant.fields {
            // For variants with one field (Send(Channel), Echo(String), etc.)
            syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                let field = fields.unnamed.first().unwrap();
                let field_type = &field.ty;

                // Extract the type as a string for pattern matching
                let type_str = quote! { #field_type }.to_string();

                // Generate different match arms based on field type
                if type_str.contains("Channel") {
                    quote! {
                        // For Channel type
                        if let komrad_core::Value::List(terms) = message.value() {
                            if terms.len() >= 2 {
                                if let Some(komrad_core::Value::Word(command)) = terms.get(0) {
                                    if command == #expected_str {
                                        if let Some(komrad_core::Value::Channel(channel)) = terms.get(1) {
                                            return Ok(#name::#variant_name(channel.clone()));
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if type_str.contains("String") {
                    quote! {
                        // For String type
                        if let komrad_core::Value::List(terms) = message.value() {
                            if terms.len() >= 2 {
                                if let Some(komrad_core::Value::Word(command)) = terms.get(0) {
                                    if command == #expected_str {
                                        if let Some(value) = terms.get(1) {
                                            match value {
                                                komrad_core::Value::String(s) => {
                                                    return Ok(#name::#variant_name(s.clone()));
                                                },
                                                komrad_core::Value::Word(w) => {
                                                    return Ok(#name::#variant_name(w.clone()));
                                                },
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Generic fallback for other types
                    quote! {
                        if let komrad_core::Value::List(terms) = message.value() {
                            if terms.len() >= 2 {
                                if let Some(komrad_core::Value::Word(command)) = terms.get(0) {
                                    if command == #expected_str {
                                        // Add specialized handling for this type
                                        // This is a placeholder for other types that may need custom conversion
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Handle other field types if needed
            _ => {
                panic!("AgentMessage enum variants must have exactly one unnamed field");
            }
        }
    });

    let expanded = quote! {
        impl std::convert::TryFrom<&komrad_core::Message> for #name {
            type Error = std::io::Error;

            fn try_from(message: &komrad_core::Message) -> Result<Self, Self::Error> {
                #(#variant_cases)*

                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Failed to parse message as {}", stringify!(#name))
                ))
            }
        }
    };

    expanded.into()
}
