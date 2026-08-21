import os, sys, json
out = {"qpa": os.environ.get("QT_QPA_PLATFORM"), "integ": os.environ.get("QT_QPA_EGLFS_INTEGRATION","-")}
try:
    from qtpy.QtWidgets import QApplication
    app = QApplication([])
    from qtpy.QtGui import QOffscreenSurface, QOpenGLContext
    s = QOffscreenSurface(); s.create()
    c = QOpenGLContext()
    if not c.create() or not c.makeCurrent(s):
        out["ok"] = False; out["err"] = "no GL context"
    else:
        from OpenGL.GL import glGetString, GL_RENDERER, GL_VERSION
        out["ok"] = True
        out["renderer"] = glGetString(GL_RENDERER).decode(errors="replace")
        out["version"] = glGetString(GL_VERSION).decode(errors="replace")
except BaseException as e:
    out["ok"] = False; out["err"] = f"{type(e).__name__}: {e}"
print("PROBE " + json.dumps(out), flush=True)
