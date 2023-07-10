use super::utils::*;

use std::collections::HashMap;

use crate::parser::{Expr, Spanned};

pub fn compile_transformation(
    transformation: Spanned<Expr>,
    _map: HashMap<String, GeometryPiece>,
) -> Result<(), Error> {
    let (expr, _span) = transformation;

    // transform is made up of reads
    if let Expr::Transform(exprs) = expr {
        for expr in exprs {
            if let Expr::Read(_, _read) = expr {}
        }
    }

    Ok(())
}
