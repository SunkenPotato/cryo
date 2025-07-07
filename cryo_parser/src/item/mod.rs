//! Items.
//!
//! Items are top-level components of cubes.

use vis::Visibility;

pub mod vis;

/// Items with an associated visibility
pub struct VisItem {
    /// The visibility of the item.
    pub vis: Visibility,
    /// The item.
    pub item: Item,
}

/// An item.
pub enum Item {}
