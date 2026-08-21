"""napari 3D+time headless render throughput. Valid only if frames are non-blank AND change."""
import os, sys, time, json
import numpy as np
from qtpy.QtWidgets import QApplication
app = QApplication.instance() or QApplication([])
sys.path.insert(0, os.path.join(os.environ["CCIA_REPO"], "napari"))
import napari_bridge, napari

R = {"gl": napari_bridge._gl_info()}
R["hw"] = "NVIDIA" in str(R["gl"].get("vendor", ""))
T, Z, Y, X = 8, 48, 512, 512
rng = np.random.default_rng(0)
vol = rng.random((T, Z, Y, X), dtype=np.float32) * 0.25
for t in range(T):                                    # blob grows AND moves -> per-frame std differs
    vol[t, 8:8 + 4 * (t + 1), 100 + 20 * t:260 + 20 * t, 150:150 + 30 * (t + 1)] = 1.0
R["volume_MB"] = round(vol.nbytes / 1e6, 1); R["shape"] = [T, Z, Y, X]

v = napari.Viewer(show=False)
v.add_image(vol, name="spike", contrast_limits=(0.0, 1.0), rendering="mip", scale=(1, 2.0, 1, 1))
SZ = dict(canvas_only=True, flash=False, size=(1080, 1920))

def measure(kind):
    v.reset_view()
    v.screenshot(canvas_only=True, flash=False)        # PLAIN first — realizes the canvas
    v.screenshot(**SZ)                                 # warm: shader compile + first upload
    stds, ts = [], []
    for t in range(T):
        v.dims.set_current_step(0, t)
        t0 = time.time()
        a = np.asarray(v.screenshot(**SZ))
        ts.append(time.time() - t0)
        stds.append(round(float(a[..., :3].std()), 2))
    med = sorted(ts)[len(ts) // 2]
    valid = all(s > 0.5 for s in stds) and len(set(stds)) >= 3
    R[kind] = {"stds": stds, "valid": valid,
               "ms_median": round(1000 * med, 1), "ms_p_max": round(1000 * max(ts), 1),
               "fps_median": round(1 / med, 1) if valid else "INVALID"}

v.dims.ndisplay = 2; measure("2d_time")
v.dims.ndisplay = 3; measure("3d_time")

v.reset_view(); v.screenshot(canvas_only=True, flash=False); v.screenshot(**SZ)
rot, rs = [], []
for a_ in range(0, 120, 15):
    v.camera.angles = (0, a_, 0)
    t0 = time.time(); im = np.asarray(v.screenshot(**SZ)); rot.append(time.time() - t0)
    rs.append(round(float(im[..., :3].std()), 2))
R["rotate_3d"] = {"stds": rs, "valid": all(s > 0.5 for s in rs) and len(set(rs)) >= 3,
                  "fps_median": round(1 / sorted(rot)[len(rot) // 2], 1)}
print("RESULT " + json.dumps(R), flush=True)
