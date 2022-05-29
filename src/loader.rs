use std::{
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

use crate::{
    instr::{Func, I32},
    mem::{Mem, Obj},
    AssetId, Name, ObjCore, OwnedName, TagId,
};

pub struct Loader {
    asset_list: Vec<*mut Obj>,
    tag_table: HashMap<OwnedName, TagId>,
    prod_table: HashMap<TagId, HashMap<String, usize>>,
    sum_table: HashMap<TagId, HashMap<String, u32>>,
    func_table: HashMap<OwnedName, DispatchList>,
}
type DispatchList = Vec<(Vec<TagExpr>, Arc<Func>)>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TagExpr {
    Has(TagId),
    And(BTreeSet<TagExpr>),
}

impl Default for Loader {
    fn default() -> Self {
        let mut loader = Self {
            asset_list: Vec::new(),
            tag_table: HashMap::new(),
            prod_table: HashMap::new(),
            sum_table: HashMap::new(),
            func_table: HashMap::new(),
        };
        let tag_unit = loader.make_tag("intrinsic.Unit");
        loader.register_prod(tag_unit, &[]);
        let tag_ref = loader.make_tag("intrinsic.Ref");
        loader.register_prod(tag_ref, &["content"]);
        let tag_bool = loader.make_tag("intrinsic.Bool");
        loader.register_sum(tag_bool, &["True", "False"]);
        loader.make_tag(I32::NAME);
        loader
    }
}

impl Loader {
    pub fn make_tag(&mut self, name: &Name) -> TagId {
        if let Some(id) = self.tag_table.get(name) {
            *id
        } else {
            let id = self.tag_table.len() as _;
            self.tag_table.insert(name.to_owned(), id);
            id
        }
    }

    pub fn register_func(&mut self, id: &Name, param_list: &[TagExpr], func: Func) {
        self.func_table
            .entry(id.to_owned())
            .or_default()
            .push((param_list.to_vec(), Arc::new(func)));
    }
}

pub enum CallArg {
    Genuine(TagId),
    Morph(Vec<TagId>),
}
impl Loader {
    pub fn dispatch_call(&self, id: &str, arg_list: &[CallArg]) -> Arc<Func> {
        for (param_list, func) in &self.func_table[id] {
            if param_list.len() != arg_list.len() {
                continue;
            }
            if arg_list
                .iter()
                .zip(param_list.iter())
                // TODO
                .all(|(arg, param)| self.is_compatible(arg, param).is_some())
            {
                return func.clone();
            }
        }
        unreachable!()
    }

    fn is_compatible(&self, arg: &CallArg, param: &TagExpr) -> Option<u32> {
        Some(u32::MAX) // TODO
    }

    pub fn register_prod(&mut self, tag: TagId, key_list: &[&str]) {
        assert!(self.tag_table.len() > tag as _);
        assert!(!self.sum_table.contains_key(&tag));
        self.prod_table.insert(
            tag,
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.to_string(), i))
                .collect(),
        );
    }

    pub fn register_sum(&mut self, tag: TagId, key_list: &[&str]) {
        assert!(self.tag_table.len() > tag as _);
        assert!(!self.prod_table.contains_key(&tag));
        self.sum_table.insert(
            tag,
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.to_string(), i as _))
                .collect(),
        );
    }

    pub fn query_tag(&self, name: &str) -> TagId {
        self.tag_table[name]
    }

    pub fn prod_size(&self, tag: TagId) -> usize {
        self.prod_table[&tag].len()
    }

    pub fn prod_offset(&self, tag: TagId, key: &str) -> usize {
        self.prod_table[&tag][key]
    }

    pub fn sum_variant(&self, tag: TagId, key: &str) -> u32 {
        self.sum_table[&tag][key]
    }

    pub fn create_asset<T>(&mut self, asset: T, mem: &Mem) -> AssetId
    where
        T: ObjCore,
    {
        let asset = mem.mutator().make(asset);
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
