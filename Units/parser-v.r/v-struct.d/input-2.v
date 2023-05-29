[heap]
pub struct Model {
    mut:
    geometry &map[u64]Geom = &map[u64]Geom{}
    layers [][]Id = [][]Id{ len: int( Layer.len ), init: []Id{} }
    last_id Id [required]
    highlighted []Id = []Id{}
    selected []Id = []Id{}
}
