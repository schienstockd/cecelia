"""Convention test: images committed to the repo stay few and small.

An image in git stays in its history for good, even after the file is deleted, and the repo is public.
Review captures for a PR therefore never come here (they stay on the developer's machine, see
`docs/todo/PR_CAPTURES_PLAN.md`); what is committed is an image a page or the app actually shows.

What this asserts:

* **Images live in the folders that show them**: `user-docs/images/` (the docs' and the app's
  screenshots, made by `pixi run doc-images`), `frontend/public/icons/` and `frontend/src/assets/`.
  Anything else must be named in `_ALLOWED_ELSEWHERE`, with the doc that cites it.
* **`user-docs/images/` stays within its limits**: no file over `_MAX_FILE_BYTES`, and not over
  `_MAX_TOTAL_BYTES` in all. The numbers sit a little above where the folder was when the check was
  added (largest file 338 kB, 1.6 MB in all), so growth fails CI rather than going unnoticed. A
  picture of the image is a JPEG and a whole-window overview too (`user-docs/shots/spec.ts` →
  `imageFile`); a UI screenshot is a PNG at the width the page shows it.

Run with `pixi run test-py`.
"""
import os
import subprocess
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

_IMAGE_EXTS = ('.png', '.jpg', '.jpeg', '.gif', '.webp', '.bmp', '.tif', '.tiff', '.ico', '.icns', '.avif', '.heic')

_IMAGE_DIRS = ('user-docs/images/', 'frontend/public/icons/', 'frontend/src/assets/')

#: Images outside `_IMAGE_DIRS` that a plan cites as its evidence. Left out of the release bundle
#: (`.github/workflows/release.yml` excludes `docs/todo` images); delete them when their plan is archived.
_ALLOWED_ELSEWHERE = {
    # docs/todo/GUI_CLARITY_AUDIT.md (RFtn9f_segment also docs/todo/JOURNEY_WORKFLOW_REVIEW.md)
    'docs/todo/gui-audit/4kS67f_gate.jpg',
    'docs/todo/gui-audit/RFtn9f_chain.jpg',
    'docs/todo/gui-audit/RFtn9f_segment.jpg',
    'docs/todo/gui-audit/RFtn9f_settings.jpg',
    'docs/todo/gui-audit/RFtn9f_welcome.jpg',
    'docs/todo/gui-audit/contact_sheet_RFtn9f.jpg',
    # docs/todo/GUI_LAYOUT_PLAN.md: renders of its spec, gui-layout/mock_final.html
    'docs/todo/gui-layout/mock_final_1.png',
    'docs/todo/gui-layout/mock_final_2.png',
    'docs/todo/gui-layout/mock_final_3.png',
    'docs/todo/gui-layout/mock_final_4.png',
    'docs/todo/gui-layout/mock_final_5.png',
}

_MAX_FILE_BYTES = 350_000
_MAX_TOTAL_BYTES = 2_000_000


def _images():
    """Every image git tracks or would add (untracked, not ignored): a new one fails before it is committed."""
    out = subprocess.check_output(['git', 'ls-files', '--cached', '--others', '--exclude-standard'], cwd=_REPO).decode()
    return sorted(p for p in out.split('\n') if p.lower().endswith(_IMAGE_EXTS) and os.path.exists(os.path.join(_REPO, p)))


class TestRepoImages(unittest.TestCase):
    def test_images_live_where_they_are_shown(self):
        stray = [p for p in _images() if not p.startswith(_IMAGE_DIRS) and p not in _ALLOWED_ELSEWHERE]
        self.assertEqual(stray, [], 'images outside ' + ', '.join(_IMAGE_DIRS) + ': a review capture belongs on the '
                         'developer\'s machine (docs/todo/PR_CAPTURES_PLAN.md), a docs image in user-docs/images/')

    def test_allowed_elsewhere_still_exist(self):
        # a stale entry would let a new file of that name back in unnoticed
        gone = sorted(p for p in _ALLOWED_ELSEWHERE if not os.path.exists(os.path.join(_REPO, p)))
        self.assertEqual(gone, [], 'drop these from _ALLOWED_ELSEWHERE')

    def test_docs_images_within_limits(self):
        sizes = {p: os.path.getsize(os.path.join(_REPO, p)) for p in _images() if p.startswith('user-docs/images/')}
        self.assertTrue(sizes, 'no images under user-docs/images/: the listing is broken')
        big = {p: n for p, n in sizes.items() if n > _MAX_FILE_BYTES}
        self.assertEqual(big, {}, f'over {_MAX_FILE_BYTES // 1000} kB each: crop tighter, show narrower, or JPEG for a picture')
        total = sum(sizes.values())
        self.assertLessEqual(total, _MAX_TOTAL_BYTES,
                             f'user-docs/images/ is {total // 1000} kB, over {_MAX_TOTAL_BYTES // 1000} kB: '
                             + ', '.join(f'{p} {n // 1000} kB' for p, n in sorted(sizes.items(), key=lambda x: -x[1])[:5]))


if __name__ == '__main__':
    unittest.main()
