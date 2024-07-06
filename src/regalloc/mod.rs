// The module generates interference graph from abstract assembly, and performs register allocation
mod allocation;
mod coaleace;
mod interference;
mod liveness;
mod prespill;

pub use allocation::Allocator;
pub use interference::InterferenceGraph;
pub use interference::Node;
