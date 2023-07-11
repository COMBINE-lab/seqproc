use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum FixedGeom {
    Len(usize),
}

#[derive(Debug, Clone)]
pub enum VariableGeom {
    Range(usize, usize),
    Unbounded,
}

#[derive(Debug, Clone)]
pub enum NucStr {
    Seq(String),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Label {
    Label(String),
}

#[derive(Debug, Clone)]
pub enum FixedGeomPiece {
    Barcode(FixedGeom, Option<Label>),
    Umi(FixedGeom, Option<Label>),
    Discard(FixedGeom, Option<Label>),
    ReadSeq(FixedGeom, Option<Label>),
    FixedSeq(NucStr, Option<Label>),
}

impl FixedGeomPiece {
    pub fn new_seq(
        sequence: NucStr,
        label: Option<Label>,
        // seq_func: Option<SequenceFunction>,
    ) -> Self {
        FixedGeomPiece::FixedSeq(sequence, label)
    }

    pub fn new_fixed(
        peice_type: char,
        label: Option<Label>,
        fixed_geom: FixedGeom,
        // fixed_func: Option<FixedFunction>,
    ) -> Self {
        match peice_type {
            'b' => FixedGeomPiece::Barcode(fixed_geom, label),
            'u' => FixedGeomPiece::Umi(fixed_geom, label),
            'x' => FixedGeomPiece::Discard(fixed_geom, label),
            'r' => FixedGeomPiece::ReadSeq(fixed_geom, label),
            _ => unreachable!(),
        }
    }

    // pub fn assign_function(
    //     self,
    //     fixed_func: Option<FixedFunction>,
    //     seq_func: Option<SequenceFunction>,
    // ) -> FixedGeomPiece {
    //     match self {
    //         FixedGeomPiece::Barcode(geom, label, _) => {
    //             FixedGeomPiece::Barcode(geom, label, fixed_func)
    //         }
    //         FixedGeomPiece::Discard(geom, label, _) => {
    //             FixedGeomPiece::Discard(geom, label, fixed_func)
    //         }
    //         FixedGeomPiece::ReadSeq(geom, label, _) => {
    //             FixedGeomPiece::ReadSeq(geom, label, fixed_func)
    //         }
    //         FixedGeomPiece::Umi(geom, label, _) => FixedGeomPiece::Umi(geom, label, fixed_func),
    //         FixedGeomPiece::FixedSeq(seq, label, _) => {
    //             FixedGeomPiece::FixedSeq(seq, label, seq_func)
    //         }
    //     }
    // }
}

#[derive(Debug, Clone)]
pub enum VariableGeomPiece {
    Barcode(VariableGeom, Option<Label>),
    Umi(VariableGeom, Option<Label>),
    Discard(VariableGeom, Option<Label>),
    ReadSeq(VariableGeom, Option<Label>),
}

impl VariableGeomPiece {
    pub fn new(
        peice_type: char,
        label: Option<Label>,
        variable_type: VariableGeom,
        // func: Option<VariableFunction>,
    ) -> Self {
        match peice_type {
            'b' => VariableGeomPiece::Barcode(variable_type, label),
            'u' => VariableGeomPiece::Umi(variable_type, label),
            'x' => VariableGeomPiece::Discard(variable_type, label),
            'r' => VariableGeomPiece::ReadSeq(variable_type, label),
            _ => unreachable!(),
        }
    }

    // pub fn assign_function(self, func: VariableFunction) -> Self {
    //     match self {
    //         VariableGeomPiece::Barcode(variable_type, label, _) => {
    //             VariableGeomPiece::Barcode(variable_type, label, Some(func))
    //         }
    //         VariableGeomPiece::Umi(variable_type, label, _) => {
    //             VariableGeomPiece::Umi(variable_type, label, Some(func))
    //         }
    //         VariableGeomPiece::Discard(variable_type, label, _) => {
    //             VariableGeomPiece::Discard(variable_type, label, Some(func))
    //         }
    //         VariableGeomPiece::ReadSeq(variable_type, label, _) => {
    //             VariableGeomPiece::ReadSeq(variable_type, label, Some(func))
    //         }
    //     }
    // }

    pub fn is_unbounded(self) -> bool {
        matches!(
            self,
            Self::Barcode(VariableGeom::Unbounded, ..)
                | Self::Discard(VariableGeom::Unbounded, ..)
                | Self::ReadSeq(VariableGeom::Unbounded, ..)
                | Self::Umi(VariableGeom::Unbounded, ..)
        )
    }
}

#[derive(Debug, Clone)]
pub struct BoundedGeom {
    pub variable: Option<VariableGeomPiece>,
    pub fixed: FixedGeomPiece,
}

#[derive(Debug, Clone)]
pub struct CompositeGeom {
    pub bounded: Vec<BoundedGeom>,
    pub variable: Option<VariableGeomPiece>,
}

#[derive(Debug, Clone)]
pub enum Description {
    CompositeGeom(CompositeGeom),
    Variable(VariableGeomPiece),
}

#[derive(Debug, Clone)]
pub struct Read {
    pub num: usize,
    pub description: Description,
}

#[derive(Debug, Clone)]
pub enum GeomPiece {
    Fixed(FixedGeomPiece),
    Variable(VariableGeomPiece),
    TransformedPiece(Function, Box<GeomPiece>),
    // FixedTransformation(FixedFunction, Box<GeomPiece>),
    // VariableTransformation(VariableFunction, Box<GeomPiece>),
}

#[derive(Debug, Clone)]
pub enum Function {
    RevComp,
    Remove,
    Reverse,
    Trim(usize),
    Pad(usize),
    Norm,
    Map(String, NucStr),
    Hamming(usize),
}

// #[derive(Debug, Clone)]
// pub enum VariableFunction {
//     Any(AnyFunction),
//     Norm,
// }

// #[derive(Debug, Clone)]
// pub enum FixedFunction {
//     Any(AnyFunction),
//     Map(String, NucStr),
//     Hamming(usize),
// }

// #[derive(Debug, Clone)]
// pub enum RangedFunction {
//     Any(AnyFunction),
//     Norm,
// }

// #[derive(Debug, Clone)]
// pub enum SequenceFunction {
//     Any(AnyFunction),
// }

// #[derive(Debug, Clone)]
// pub enum AnyFunction {
//     RevComp,
//     Remove,
//     Reverse,
//     Trim(usize),
//     Pad(usize),
// }

#[derive(Debug, Clone)]
pub struct Definition {
    pub label: Label,
    pub geom_piece: GeomPiece,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub definitions: HashMap<Label, GeomPiece>,
    pub geometry: Vec<Read>,
    pub transformation: Option<Vec<GeomPiece>>,
}

/*
   There needs to be a logical nested structure for expr
*/

// pub enum Expr {
//     GeomPiece(GeomPiece),
//     FixedTransformation(FixedFunction, Box<Expr>),
//     VariableTransformation(VariableFunction, Box<Expr>),
// }
