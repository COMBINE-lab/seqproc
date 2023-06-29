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

#[derive(Debug, Clone)]
pub enum Label {
    Label(String),
}

#[derive(Debug, Clone)]
pub enum FixedGeomPiece {
    Barcode(FixedGeom, Option<Label>),
    Umi(FixedGeom, Option<Label>),
    Discard(FixedGeom, Option<Label>),
    ReadSeq(FixedGeom, Option<Label>),
    Fixed(NucStr, Option<Label>),
}

impl FixedGeomPiece {
    pub fn new(
        peice_type: char,
        label: Option<Label>,
        fixed_geom: Option<FixedGeom>,
        sequence: Option<NucStr>,
    ) -> Self {
        match peice_type {
            'b' => FixedGeomPiece::Barcode(fixed_geom.unwrap(), label),
            'u' => FixedGeomPiece::Umi(fixed_geom.unwrap(), label),
            'x' => FixedGeomPiece::Discard(fixed_geom.unwrap(), label),
            'r' => FixedGeomPiece::ReadSeq(fixed_geom.unwrap(), label),
            'f' => FixedGeomPiece::Fixed(sequence.unwrap(), label),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum VariableGeomPiece {
    Barcode(VariableGeom, Option<Label>),
    Umi(VariableGeom, Option<Label>),
    Discard(VariableGeom, Option<Label>),
    ReadSeq(VariableGeom, Option<Label>),
}

impl VariableGeomPiece {
    pub fn new(peice_type: char, label: Option<Label>, variable_type: VariableGeom) -> Self {
        match peice_type {
            'b' => VariableGeomPiece::Barcode(variable_type, label),
            'u' => VariableGeomPiece::Umi(variable_type, label),
            'x' => VariableGeomPiece::Discard(variable_type, label),
            'r' => VariableGeomPiece::ReadSeq(variable_type, label),
            _ => unreachable!(),
        }
    }

    pub fn is_unbounded(self) -> bool {
        matches!(self,
            Self::Barcode(VariableGeom::Unbounded, _)
            | Self::Discard(VariableGeom::Unbounded, _)
            | Self::ReadSeq(VariableGeom::Unbounded, _)
            | Self::Umi(VariableGeom::Unbounded, _) 
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
pub enum Functions {
    Pad(usize),
    Map(NucStr, String, NucStr),
    Trim,
    Reverse,
    RevComp,
}
