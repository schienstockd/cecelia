struct PickData {
  focus:    u32,
  contour:  u32,
  reserved0: u32,
  reserved1: u32,
  bits:     array<u32, ${PICK_BITSET_WORDS}>,
};
@group(0) @binding(${PICK_BINDING}) var<storage, read> pick: PickData;

fn labInPick(id: u32) -> bool {
  if (id == 0u || id >= ${PICK_BITSET_CAPACITY}u) { return false; }
  let w = pick.bits[id >> 5u];
  return (w & (1u << (id & 31u))) != 0u;
}
fn labIsFocus(id: u32) -> bool { return id != 0u && id == pick.focus; }
fn labPickContourPx() -> i32 { return i32(pick.contour); }
