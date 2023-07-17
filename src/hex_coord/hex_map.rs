use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use super::HexVector;

use serde::{
    de::{DeserializeSeed, Visitor},
    Deserialize, Serialize,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HexMap<T> {
    map: HashMap<HexVector, T>,
}

impl<T: Serialize> Serialize for HexMap<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        MapSerializer(&self.map).serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for HexMap<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let map = MapDeserializer::default().deserialize(deserializer)?;
        Ok(HexMap { map })
    }
}

impl<T: Default> HexMap<T> {
    pub fn new(size: usize) -> Self {
        Self::new_with_init(size, |_| T::default())
    }
}

impl<T> HexMap<T> {
    pub fn new_with_init<F>(size: usize, mut init_fn: F) -> Self
    where
        F: FnMut(HexVector) -> T,
    {
        let size = size as isize;
        let mut map = HashMap::new();
        for q in -size..=size {
            let r1 = (-size).max(-q - size);
            let r2 = size.max(-q + size);
            for r in r1..=r2 {
                let pos = HexVector::new_axial(q, r);
                map.insert(pos, init_fn(pos));
            }
        }
        HexMap { map }
    }

    pub fn get(&self, pos: HexVector) -> Option<&T> {
        self.map.get(&pos)
    }

    pub fn get_mut(&mut self, pos: HexVector) -> Option<&mut T> {
        self.map.get_mut(&pos)
    }

    pub fn get_keys(&self) -> impl Iterator<Item = HexVector> + '_ {
        self.map.keys().copied()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            iter: self.map.iter(),
        }
    }
}

impl<T> IntoIterator for HexMap<T> {
    type Item = (HexVector, T);
    type IntoIter = std::collections::hash_map::IntoIter<HexVector, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

pub struct Iter<'a, T> {
    iter: std::collections::hash_map::Iter<'a, HexVector, T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (HexVector, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let (key, value) = self.iter.next()?;
        Some((*key, value))
    }
}

impl<'a, T> IntoIterator for &'a HexMap<T> {
    type Item = (HexVector, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

struct MapSerializer<'a, K, V>(&'a HashMap<K, V>);

impl<'a, K, V> Serialize for MapSerializer<'a, K, V>
where
    K: Serialize,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(self.0.iter())
    }
}

struct MapDeserializer<K, V> {
    marker: PhantomData<(K, V)>,
}

impl<K, V> Default for MapDeserializer<K, V> {
    fn default() -> Self {
        MapDeserializer {
            marker: PhantomData,
        }
    }
}

impl<'de, K, V> DeserializeSeed<'de> for MapDeserializer<K, V>
where
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
{
    type Value = HashMap<K, V>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de, K, V> Visitor<'de> for MapDeserializer<K, V>
where
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
{
    type Value = HashMap<K, V>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a sequence of {{key, value}} pairs")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut map = HashMap::new();
        while let Some((key, value)) = seq.next_element::<(K, V)>()? {
            map.insert(key, value);
        }
        Ok(map)
    }
}
