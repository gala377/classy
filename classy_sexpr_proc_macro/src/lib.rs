extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::{
    braced, bracketed, parenthesized, parse_macro_input, token, Expr, Ident, LitBool, LitInt,
    LitStr, Result, Token,
};

struct SExprParser {
    final_expr: Expr,
}

impl Parse for SExprParser {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let mut quoted_list = quote! {
                let mut res = Vec::new();
            };
            while !content.is_empty() {
                if content.peek(Token![@]) {
                    content.parse::<Token![@]>().unwrap();
                    if content.peek(token::Brace) {
                        let inner;
                        braced!(inner in content);
                        let inner = inner.parse::<Expr>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend((#inner).into_iter().map(|x| x.to_sexpr()));
                        };
                        continue;
                    } else {
                        let ident = content.parse::<Ident>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend(#ident.into_iter().map(|x| x.to_sexpr()));
                        }
                    }
                } else {
                    let res: SExprParser = content.parse()?;
                    let inner = res.final_expr;
                    quoted_list = quote! {
                        #quoted_list
                        res.push(#inner);
                    };
                };
            }
            let final_expr = quote! {
                {
                    #quoted_list
                    classy_sexpr::SExpr::List(res)
                }
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }

        if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let mut quoted_list = quote! {
                let mut res = Vec::new();
            };
            while !content.is_empty() {
                if content.peek(Token![@]) {
                    content.parse::<Token![@]>().unwrap();
                    if content.peek(token::Brace) {
                        let inner;
                        braced!(inner in content);
                        let inner = inner.parse::<Expr>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend((#inner).into_iter().map(|x| x.to_sexpr()));
                        };
                        continue;
                    } else {
                        let ident = content.parse::<Ident>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend(#ident.into_iter().map(|x| x.to_sexpr()));
                        }
                    }
                } else {
                    let res: SExprParser = content.parse()?;
                    let inner = res.final_expr;
                    quoted_list = quote! {
                        #quoted_list
                        res.push(#inner);
                    };
                };
            }
            let final_expr = quote! {
                {
                    #quoted_list
                    classy_sexpr::SExpr::List(res)
                }
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }

        if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            let mut quoted_list = quote! {
                let mut res = Vec::new();
            };
            while !content.is_empty() {
                if content.peek(Token![@]) {
                    content.parse::<Token![@]>().unwrap();
                    if content.peek(token::Brace) {
                        let inner;
                        braced!(inner in content);
                        let inner = inner.parse::<Expr>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend((#inner).into_iter().map(|x| x.to_sexpr()));
                        };
                        continue;
                    } else {
                        let ident = content.parse::<Ident>().unwrap();
                        quoted_list = quote! {
                            #quoted_list
                            res.extend(#ident.into_iter().map(|x| x.to_sexpr()));
                        }
                    }
                } else {
                    let res: SExprParser = content.parse()?;
                    let inner = res.final_expr;
                    quoted_list = quote! {
                        #quoted_list
                        res.push(#inner);
                    };
                };
            }
            let final_expr = quote! {
                {
                    #quoted_list
                    classy_sexpr::SExpr::List(res)
                }
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(LitInt) {
            let num = input.parse::<LitInt>().unwrap();
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Number(#num))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(LitStr) {
            let s = input.parse::<LitStr>().unwrap();
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::String(#s.to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(LitBool) {
            let b = input.parse::<LitBool>().unwrap();
            let b = if b.value { "true" } else { "false" };
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Symbol(#b.to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(Token![$]) {
            input.parse::<Token![$]>().unwrap();
            if input.peek(token::Brace) {
                let content;
                braced!(content in input);
                let inner = content.parse::<Expr>().unwrap();
                let final_expr = quote! {
                    (#inner).to_sexpr()
                }
                .into();
                let final_expr = syn::parse::<Expr>(final_expr).unwrap();
                return Ok(Self { final_expr });
            }
            let ident = input.parse::<Ident>().unwrap();
            let final_expr = quote! {
                #ident.to_sexpr()
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(Token![#]) {
            input.parse::<Token![#]>().unwrap();
            let id = input.parse::<Ident>().unwrap();
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Symbol(#id.to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if input.peek(Token![@]) {
            let ret = quote! {
                compile_error!("splicing operator @ can only be used inside a list");
            };
            let final_expr = syn::parse::<Expr>(ret.into()).unwrap();
            return Ok(Self { final_expr });
        }
        if let Ok(ident) = input.call(Ident::parse_any) {
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Symbol(stringify!(#ident).to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        Err(input.error("Not a valid sexpr"))
    }
}

#[proc_macro]
pub fn sexpr(tokens: TokenStream) -> TokenStream {
    let SExprParser { final_expr: parsed } = parse_macro_input!(tokens as SExprParser);
    quote! {
        #parsed
    }
    .into()
}
