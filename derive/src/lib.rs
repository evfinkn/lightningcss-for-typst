use proc_macro::TokenStream;

mod parse;
mod to_typst;
mod visit;

#[proc_macro_derive(Visit, attributes(visit, skip_visit, skip_type, visit_types))]
pub fn derive_visit_children(input: TokenStream) -> TokenStream {
  visit::derive_visit_children(input)
}

#[proc_macro_derive(Parse, attributes(css))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
  parse::derive_parse(input)
}

#[proc_macro_derive(ToTypst, attributes(css))]
pub fn derive_to_typst(input: TokenStream) -> TokenStream {
  to_typst::derive_to_typst(input)
}
