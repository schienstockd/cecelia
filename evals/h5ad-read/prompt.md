Add a small helper in `python/cecelia/analysis_scratch/read_track_speed.py` that reads a `.h5ad`
at a given path, filters to a given list of `label` ids, and returns the `live.cell.speed`
column as a `pandas.Series`. It'll be called from a notebook.

Sketch of the desired signature:

    def read_track_speed(path: str, label_ids: list[int]) -> pd.Series: ...

Ship the file with the function + any imports it needs. No tests, no docstrings beyond a
one-liner. Don't commit — just write the file.
