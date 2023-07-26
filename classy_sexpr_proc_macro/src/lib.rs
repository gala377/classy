extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_macro_input, token, Expr, Ident, LitBool, LitInt, LitStr, Result};

struct SExprParser {
    final_expr: Expr,
}

impl Parse for SExprParser {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Paren) {
            let mut inner = Vec::new();
            let content;
            parenthesized!(content in input);
            while !content.is_empty() {
                let res: SExprParser = content.parse()?;
                inner.push(res);
            }
            let inner = inner.into_iter().map(|x| x.final_expr).collect::<Vec<_>>();
            let final_expr = quote! {
                classy_sexpr::SExpr::List(vec![#(#inner),*])
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if let Ok(s) = input.parse::<LitStr>() {
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::String(#s.to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if let Ok(b) = input.parse::<LitBool>() {
            let b = if b.value { "true" } else { "false" };
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Symbol(#b.to_string()))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if let Ok(num) = input.parse::<LitInt>() {
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Number(#num))
            }
            .into();
            let final_expr = syn::parse::<Expr>(final_expr).unwrap();
            return Ok(Self { final_expr });
        }
        if let Ok(ident) = input.parse::<Ident>() {
            let final_expr = quote! {
                classy_sexpr::SExpr::Atom(classy_sexpr::Atom::Symbol(#ident.to_string()))
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
