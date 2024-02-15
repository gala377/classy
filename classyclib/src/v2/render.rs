use std::fmt;

use maud::{html, Escaper, Markup, Render};
use std::fmt::Write;

use crate::v2::knowledge::{
    self, ClassInfo, ClassMethodBlock, DefinitionId, Id, LocalId, MethodHandle, TypeId,
    CURRENT_PACKAGE_ID,
};
use crate::v2::ty::Type;

use super::knowledge::PackageId;

pub fn render_db(db: &knowledge::Database, path: impl AsRef<std::path::Path>) {
    render_types_page(&db, &path.as_ref().join("types.html"));
    render_classes(db, &path.as_ref().join("classes.html"));
    render_definitions(db, &path.as_ref().join("definitions.html"));
}

fn menu() -> Markup {
    html! {
        div class="navbar" {
            a href="types.html" { "Types" }
            a href="classes.html" { "Classes" }
            a href="definitions.html" { "Definitions" }
        }
    }
}

fn css() -> Markup {
    html! {
        link rel="stylesheet" type="text/css" href=("style.css");
    }
}

fn render_definitions(db: &knowledge::Database, path: &std::path::Path) {
    let markup = html! {
        (header())
        table {
            thead {
                tr {
                    th { "Id" }
                    th { "Name" }
                    th { "Kind" }
                    th { "Type" }
                    th { "File" }
                }
            }
            tbody {
                @for (LocalId(DefinitionId(id)), def) in &db.definitions {
                    tr {
                        td { (id) }
                        (render_def(db, def))
                    }
                }
            }
        }
    };
    std::fs::write(path, markup.into_string()).unwrap();
}

fn render_def(
    _db: &knowledge::Database,
    knowledge::Definition {
        name,
        kind,
        constraints: _,
        ty,
        file,
        implicit_imports: _,
    }: &knowledge::Definition,
) -> Markup {
    html! {
        td { (name) }
        td { (Debug::from(kind)) }
        td { a href=(format!("types.html#{}", ty.0.0)) { (Debug::from(ty)) } }
        td { (Debug::from(file)) }
    }
}

fn render_types_page(db: &knowledge::Database, path: &std::path::Path) {
    let markup = html! {
        (header())
        body onload="highlightRow();" {
            table {
                thead {
                    tr {
                        th { "Id" }
                        th { "Kind" }
                        th { "Type" }
                    }
                }
                tbody {
                    @for (id, ty) in &db.typeid_to_type {
                        tr id=(id.0.0) {
                            td { ( bare_tid(id)) }
                            td { (type_kind(ty)) }
                            td { (render_type(db, ty)) }
                        }
                    }
                }
            }
        }
    };
    std::fs::write(path, markup.into_string()).unwrap();
}

fn header() -> Markup {
    html! {
        script src="scripts.js" {}
        (css())
        (menu())
    }
}

fn render_classes(db: &knowledge::Database, path: &std::path::Path) {
    let markup = html! {
        (header())
        table {
            thead {
                tr {
                    th { "Id" }
                    th { "Name" }
                    th { "Arguments" }
                    th { "Static methods" }
                    th { "Method blocks" }
                }
            }
            tbody {
                @for (LocalId(DefinitionId(id)), class) in &db.classes {
                    tr {
                        td { (id) }
                        (render_class(db, class))
                    }
                }
            }
        }
    };
    std::fs::write(path, markup.into_string()).unwrap();
}

fn render_class(
    db: &knowledge::Database,
    ClassInfo {
        name,
        static_methods,
        method_blocks,
        arguments,
    }: &ClassInfo,
) -> Markup {
    html! {
        td { (name) }
        td {
            @for name in arguments {
                (name)
                ", "
            }
        }
        td {
            @for MethodHandle { name, definition } in static_methods {
                span {
                    (name)
                    "=>"
                    (Debug::from(definition))
                }
                br;
            }
        }
        td {
            @for ClassMethodBlock { receiver, methods }  in method_blocks {

                    @if receiver.package == CURRENT_PACKAGE_ID {
                        a href=(format!("types.html#{}", receiver.id.0)) { (format_id(db, receiver)) }
                    } @else {
                        (format_id(db, receiver))
                    }
                    "{"
                    br;
                    @for MethodHandle { name, definition } in methods {
                        span {
                            (name)
                            "=>"
                            (Debug::from(definition))
                        }
                        br;
                    }
                    "}"
                    br;
            }
        }
    }
}

fn render_type(db: &knowledge::Database, ty: &Type) -> Markup {
    match ty {
        Type::Int => html! { "Int" },
        Type::UInt => html! { "UInt" },
        Type::Bool => html! { "Bool" },
        Type::String => html! { "String" },
        Type::Float => html! { "Float" },
        Type::Unit => html! { "Unit" },
        Type::Byte => html! { "Byte" },
        Type::Struct { def, fields } => {
            html! {
                "struct ("
                (format_id(db, def))
                ") {"
                br;
                @for (name, t) in fields {
                    (name)
                    ": "
                    (render_type(db, t))
                    "; "
                    br;
                }
                "}"
            }
        }
        Type::ADT { def, constructors } => {
            html! {
                "adt "
                (Debug::from(def))
                " {"
                @for (name, t) in constructors {
                    (name)
                    ": "
                    (render_type(db, t))
                    "; "
                }
                "}"
            }
        }
        Type::Function { args, ret } => {
            html! {
                "("
                @for t in args {
                    (render_type(db, t))
                    @if t != args.last().unwrap() {
                        ", "
                    }
                }
                ") -> "
                (render_type(db, ret))
            }
        }
        Type::Tuple(inner) => {
            html! {
                "("
                @for t in inner {
                    (render_type(db, t))
                    @if t != inner.last().unwrap() {
                        ", "
                    }
                }
                ")"
            }
        }
        Type::Array(inner) => {
            html! {
                "["
                (render_type(db, inner))
                "]"
            }
        }
        Type::Alias(Id { package, id }) => {
            html! {
                "&"
                (package_name(db, package.clone()))
                "["
                @if *package == CURRENT_PACKAGE_ID {
                    a href=(format!("types.html#{}", id.0)) onclick="updateURL(this.href)(event)" { (Debug::from(id)) }
                } @else {
                    (Debug::from(id))
                }
                "]"
            }
        }
        Type::Divergent => {
            html! { "!" }
        }
        Type::ToInfere => {
            html! { "_" }
        }
        Type::Scheme { prefex, typ } => {
            html! {
                "∀["
                @for t in prefex {
                    (t)
                    @if t != prefex.last().unwrap() {
                        ", "
                    }
                }
                "] "
                (render_type(db, typ))
            }
        }
        Type::App { typ, args } => {
            html! {
                (render_type(db, typ))
                "<"
                @for t in args {
                    (render_type(db, t))
                    @if t != args.last().unwrap() {
                        ", "
                    }
                }
                ">"
            }
        }
        Type::Generic(shift, index) => {
            html! {
                "@("
                (shift.0)
                ", "
                (index)
                ")"
            }
        }
        Type::Fresh(id) => {
            html! {
                "?"
                (id)
            }
        }
    }
}

fn type_kind(ty: &Type) -> Markup {
    match ty {
        Type::Int => html! { "Int" },
        Type::UInt => html! { "UInt" },
        Type::Bool => html! { "Bool" },
        Type::String => html! { "String" },
        Type::Float => html! { "Float" },
        Type::Unit => html! { "Unit" },
        Type::Byte => html! { "Byte" },
        Type::Struct { .. } => html! { "Struct" },
        Type::ADT { .. } => html! { "ADT" },
        Type::Function { .. } => html! { "Function" },
        Type::Tuple(_) => html! { "Tuple" },
        Type::Array(_) => html! { "Array" },
        Type::Alias(_) => html! { "Alias" },
        Type::Divergent => html! { "Divergent" },
        Type::ToInfere => html! { "ToInfere" },
        Type::Scheme { .. } => html! { "Scheme" },
        Type::App { .. } => html! { "App" },
        Type::Generic(_, _) => html! { "Generic" },
        Type::Fresh(_) => html! { "Fresh" },
    }
}

fn bare_tid(id: &LocalId<knowledge::TypeId>) -> Markup {
    html! {
        (id.0.0)
    }
}

fn format_id<T: fmt::Debug + Clone>(db: &knowledge::Database, id: &Id<T>) -> Markup {
    html! {
        (package_name(db, id.package.clone()))
        "["
        (Debug::from(id.id.clone()))
        "]"
    }
}

fn package_name(db: &knowledge::Database, id: PackageId) -> Markup {
    let package_name = if id == CURRENT_PACKAGE_ID {
        "PKG"
    } else {
        &db.get_package(id).name
    };
    html! {
        (package_name)
    }
}

struct Debug<T: fmt::Debug>(T);

impl<T: fmt::Debug> From<T> for Debug<T> {
    fn from(t: T) -> Debug<T> {
        Debug(t)
    }
}

impl<T: fmt::Debug> Render for Debug<T> {
    fn render_to(&self, output: &mut String) {
        let mut escaper = Escaper::new(output);
        write!(escaper, "{:?}", self.0).unwrap();
    }
}
