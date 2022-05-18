use ecosphere::{
    instr::{FuncBuilder, Instr, Val},
    obj::Obj,
};

fn main() {
    let eq = || "eq".to_string();
    let add = || "add".to_string();

    let mut func = FuncBuilder::default();
    let b1 = func.push_block();
    func.with_block(b1);
    let i1 = func.push_instr(Instr::Alloc(Val::Const(Obj::I32(1))));
    let i2 = func.push_instr(Instr::Alloc(Val::Const(Obj::I32(1))));
    let i3 = func.push_instr(Instr::Alloc(Val::Const(Obj::I32(0))));
    let b2 = func.push_block();
    func.push_instr(Instr::Br(Val::Const(Obj::bool_true()), b2, b2));
    func.with_block(b2);
    let i4 = func.push_instr(Instr::Load(Val::Instr(i1)));
    let i5 = func.push_instr(Instr::Op(eq(), vec![Val::Instr(i4), Val::Arg(0)]));
    let b3 = func.push_block();
    let b4 = func.push_block();
    func.push_instr(Instr::Br(Val::Instr(i5), b3, b4));
    func.with_block(b3);
    let i6 = func.push_instr(Instr::Load(Val::Instr(i2)));
    let i7 = func.push_instr(Instr::Load(Val::Instr(i3)));
    let i8 = func.push_instr(Instr::Op(add(), vec![Val::Instr(i6), Val::Instr(i7)]));
    func.push_instr(Instr::Store(Val::Instr(i2), Val::Instr(i8)));
    func.push_instr(Instr::Store(Val::Instr(i3), Val::Instr(i6)));
    let i9 = func.push_instr(Instr::Op(
        add(),
        vec![Val::Instr(i1), Val::Const(Obj::I32(1))],
    ));
    func.push_instr(Instr::Store(Val::Instr(i1), Val::Instr(i9)));
    func.push_instr(Instr::Br(Val::Const(Obj::bool_true()), b2, b2));
    func.with_block(b4);
    let i10 = func.push_instr(Instr::Load(Val::Instr(i2)));
    func.push_instr(Instr::Ret(Val::Instr(i10)));
    let func = func.finish();
    print!("{func}");
}
