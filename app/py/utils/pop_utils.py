"""
Population accessor for Python tasks / notebooks.

Reduced from the old `inst/py/pop_utils.py`: there is **no per-pop CSV and no Python gate
engine** any more (docs/POPULATION.md). `pop_df` keeps roughly its old call shape, but
resolves *membership* via the Julia API (`CeceliaClient`) and reads the bulk measurement
columns *locally* from the H5AD (`LabelPropsUtils`). One code path for notebook dev and
shipped modules.

A `live`/`clust` pop can span multiple value_names (its path prefix names the segmentation);
that pooling is wired in Julia's `pop_df` and will be surfaced here when those creation UIs
land. This phase handles `flow` (one value_name per call).
"""
import pandas as pd


class PopUtils:
    def __init__(self, client=None):
        # client = CeceliaClient(base_url, project_uid, image_uid)
        self.client = client

    def pop_df(self, task_dir, label_props_utils, pop_type, pops=(), cols=(),
               value_name: str = "default", unique_labels: bool = True,
               invert: bool = False):
        """Return a DataFrame of the cells in `pops` with a `pop` + `value_name` column and
        the requested `cols` (read locally from the H5AD). Membership comes from the API.

        `task_dir` is accepted for signature compatibility (the columns are read through the
        provided `label_props_utils`, which already knows the task dir)."""
        if self.client is None:
            raise RuntimeError("PopUtils needs a CeceliaClient to resolve membership")
        if isinstance(pops, str):
            pops = [pops]
        pops = list(pops)
        cols = list(cols)

        membership = self.client.cells_in_pops(pop_type, pops, value_name=value_name)

        frames = []
        for pop, labels in membership.items():
            view = label_props_utils.label_props_view(value_name=value_name)
            if cols:
                view.view_cols(cols)
            if invert:
                keep = set(int(x) for x in view.labels()) - set(int(x) for x in labels)
                df = view.filter_by_label(keep).as_df()
            else:
                df = view.filter_by_label(labels).as_df()
            view.close()
            df["value_name"] = value_name
            df["pop"] = pop
            frames.append(df)

        if not frames:
            return None
        out = pd.concat(frames, ignore_index=True)
        if unique_labels and len(out) > 0:
            # most-specific pop wins (deepest path), matching Julia pop_df dedup
            out["__depth"] = out["pop"].str.count("/")
            out = (out.sort_values(["value_name", "label", "__depth"])
                      .drop_duplicates(["value_name", "label"], keep="last")
                      .drop(columns="__depth")
                      .reset_index(drop=True))
        return out
