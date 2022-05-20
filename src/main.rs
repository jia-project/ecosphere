use ecosphere::instr::{FuncBuilder, Instr, Val, ValConst};

fn main() {
    let eq = || "eq".to_string();
    let add = || "add".to_string();

    let mut func = FuncBuilder::default();
    let i1 = func.push_instr(Instr::Alloc(Val::Const(ValConst::I32(1))));
    let i2 = func.push_instr(Instr::Alloc(Val::Const(ValConst::I32(1))));
    let i3 = func.push_instr(Instr::Alloc(Val::Const(ValConst::I32(0))));
    let b1 = func.push_block();
    func.push_instr(Instr::Br(Val::Const(ValConst::Bool(true)), b1, b1));
    func.with_block(b1);
    let i4 = func.push_instr(Instr::Load(i1));
    let i5 = func.push_instr(Instr::Op(eq(), vec![i4, Val::Arg(0)]));
    let b2 = func.push_block();
    let b3 = func.push_block();
    func.push_instr(Instr::Br(i5, b2, b3));
    func.with_block(b2);
    let i6 = func.push_instr(Instr::Load(i2));
    let i7 = func.push_instr(Instr::Load(i3));
    let i8 = func.push_instr(Instr::Op(add(), vec![i6, i7]));
    func.push_instr(Instr::Store(i2, i8));
    func.push_instr(Instr::Store(i3, i6));
    let i9 = func.push_instr(Instr::Op(add(), vec![i1, Val::Const(ValConst::I32(1))]));
    func.push_instr(Instr::Store(i1, i9));
    func.push_instr(Instr::Br(Val::Const(ValConst::Bool(true)), b1, b1));
    func.with_block(b3);
    let i10 = func.push_instr(Instr::Load(i2));
    func.push_instr(Instr::Ret(i10));
    let func = func.finish();
    print!("{func}");
}
