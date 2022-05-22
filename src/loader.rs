use std::{collections::HashMap, sync::Arc};

use crate::{
    instr::Func,
    mem::{Mem, Obj},
    obj::Native,
    AssetId, HeaderId, Name, ObjCore,
};

pub struct Loader {
    asset_list: Vec<*mut Obj>,
    header_table: HashMap<Name, HeaderId>,
    key_table: Vec<HashMap<String, u32>>,
}

impl Default for Loader {
    fn default() -> Self {
        let mut loader = Self {
            asset_list: Vec::new(),
            header_table: HashMap::new(),
            key_table: Vec::new(),
        };
        loader.register_header("intrinsic.Unit".to_string(), &[]);
        loader.register_header("intrinsic.Ref".to_string(), &["content".to_string()]);
        loader.register_header(
            "intrinsic.Bool".to_string(),
            &["True".to_string(), "False".to_string()],
        );
        loader
    }
}

impl Loader {
    pub fn register_func(&mut self, id: Name, func: Func) {
        //
    }

    pub fn dispatch_call(&self, id: &Name, arg_list: &[&dyn ObjCore]) -> Arc<Func> {
        todo!()
    }

    pub fn register_header(&mut self, name: Name, key_list: &[String]) -> HeaderId {
        let id = self.key_table.len() as _;
        self.header_table.insert(name, id);
        self.key_table.push(
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.clone(), i as _))
                .collect(),
        );
        id
    }

    pub fn query_type(&self, name: &Name) -> HeaderId {
        self.header_table[name]
    }

    pub fn query_header_size(&self, header: HeaderId) -> usize {
        self.key_table[header as usize].len()
    }

    pub fn query_header_index(&self, header: HeaderId, key: &str) -> u32 {
        self.key_table[header as usize][key]
    }

    // query tag

    pub fn install_asset<T>(&mut self, asset: T, mem: &Mem) -> AssetId
    where
        Native<T>: ObjCore,
    {
        let asset = mem.mutator().alloc(Native(asset));
        let id = self.asset_list.len();
        self.asset_list.push(asset);
        id as _
    }

    pub fn query_asset(&self, id: AssetId) -> *mut Obj {
        self.asset_list[id as usize]
    }

    pub fn trace(&self, mark: impl FnMut(*mut Obj)) {
        self.asset_list.iter().copied().for_each(mark);
    }
}
