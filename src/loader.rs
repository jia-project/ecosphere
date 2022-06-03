use std::{collections::HashMap, sync::Arc};

use crate::{
    instr::Func,
    interp::Interp,
    mem::{Mem, Obj},
    AssetId, Name, ObjCore, OwnedName,
};

pub type TagId = u32;

pub struct Loader {
    asset_list: Vec<*mut Obj>,
    tag_table: HashMap<OwnedName, TagId>,
    prod_table: HashMap<TagId, HashMap<String, usize>>,
    sum_table: HashMap<TagId, HashMap<String, u32>>,
    func_table: HashMap<(OwnedName, usize), DispatchList>,
}
type DispatchList = Vec<(Vec<Param>, Arc<Func>)>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Param {
    Genuine(OwnedName),
    Match(MatchExpr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchExpr {
    Has(OwnedName),
    Anything, // eventually should be And([]), make it specific before match
    // dispatch is implemented
    And(Vec<MatchExpr>),
}

unsafe impl Send for Loader {}
unsafe impl Sync for Loader {}

impl Default for Loader {
    fn default() -> Self {
        let mut loader = Self {
            asset_list: Vec::new(),
            tag_table: HashMap::new(),
            prod_table: HashMap::new(),
            sum_table: HashMap::new(),
            func_table: HashMap::new(),
        };
        Interp::load(&mut loader);
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

    pub fn register_func(&mut self, id: &Name, param_list: &[Param], func: Func) {
        // println!("{id}{param_list:?}");
        // println!("{func}");
        self.func_table
            .entry((id.to_owned(), param_list.len()))
            .or_default()
            .push((param_list.to_vec(), Arc::new(func)));
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallArg {
    Genuine(TagId),
    Morph(Vec<TagId>),
}

impl Loader {
    pub fn dispatch_call(&self, id: &Name, arg_list: &[CallArg]) -> Arc<Func> {
        let candidate_list =
            if let Some(candidate_list) = self.func_table.get(&(id.to_owned(), arg_list.len())) {
                candidate_list
            } else {
                panic!("no function for dispatch {id}/{}", arg_list.len());
            };
        let mut genuine_candidate = None;
        let mut anything_candidate = None;
        for (param_list, func) in candidate_list {
            if param_list.len() != arg_list.len() {
                continue;
            }
            let mut genuine_dispatch = true;
            let mut anything_dispatch = true;
            for (param, arg) in param_list.iter().zip(arg_list.iter()) {
                match (param, arg) {
                    (Param::Match(MatchExpr::Anything), _) => {
                        genuine_dispatch = false;
                    }
                    (Param::Genuine(name), CallArg::Genuine(tag))
                        if self.query_tag(name) == *tag => {}
                    _ => {
                        genuine_dispatch = false;
                        anything_dispatch = false;
                        // TODO check match dispatch
                    }
                }
            }
            if genuine_dispatch {
                let prev = genuine_candidate.replace(func);
                assert!(prev.is_none());
            } else if anything_dispatch && genuine_candidate.is_none() {
                let prev = anything_candidate.replace(func);
                assert!(prev.is_none());
            }
        }
        if let Some(func) = genuine_candidate {
            func.clone()
        } else if let Some(func) = anything_candidate {
            func.clone()
        } else {
            panic!("dispatch failed for function {id}/{}", arg_list.len());
        }
    }

    pub fn register_prod(&mut self, name: &Name, key_list: &[&str]) {
        let tag = self.make_tag(name);
        assert!(!self.prod_table.contains_key(&tag));
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

    pub fn register_sum(&mut self, name: &Name, key_list: &[&str]) {
        let tag = self.make_tag(name);
        assert!(!self.prod_table.contains_key(&tag));
        assert!(!self.sum_table.contains_key(&tag));
        self.sum_table.insert(
            tag,
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.to_string(), i as _))
                .collect(),
        );
    }

    pub fn query_tag(&self, name: &Name) -> TagId {
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

    pub fn create_asset<T: ObjCore + 'static>(&mut self, asset: T, mem: &Mem) -> AssetId {
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
