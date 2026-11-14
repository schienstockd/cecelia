// The screenshots of user-docs/using-ai.md (the page itself: frontend/src/utils/usingAiDoc.ts). The
// button text a shot checks is imported from the app, as the page's is.

import type { ShotSet } from './spec.ts'
import { AI_OPT_IN_COPY } from '../../frontend/src/utils/aiSwitch.ts'

export const USING_AI_SHOTS: ShotSet = {
  id: 'using-ai',
  page: 'using-ai.md',
  themes: ['light'],
  // newcomer: before turning AI on. sam: a new member after it (AI on, not logged in). default
  // asks the one saved question (the terminal's login). One lab image to point at.
  fixture: {
    projects: { ln: {
      source: '4rNbMp', images: { FtGoJO: 'Lymphoid tissue, LCMV' }, name: 'Lymphoid tissue',
      kiwi: { image: 'FtGoJO', question: 'What are this image’s channels, pixel size and z-depth?' },
    } },
    profiles: [{ name: 'newcomer', ai: false }, { name: 'sam', ai: true }, { name: 'default', ai: true }],
  },
  shots: [
    {
      // The dialog, not the switch: what the reader must read before saying yes is the point. Cropped
      // from "Keep in mind" down — what is sent is already the page's table.
      id: 'ai-turn-on',
      profile: 'newcomer',
      actions: [{ click: 'header.profile' }, { click: 'header.preferences' }, { click: 'prefs.kiwi' },
                { click: 'prefs.aiFeatures' }, { waitFor: 'aiOptIn.turnOn' }],
      point: { anchor: 'aiOptIn.turnOn', label: 'Read it, then Turn on' },
      crop: ['aiOptIn.risks', 'aiOptIn.turnOn'],
      expect: [{ anchor: 'aiOptIn.turnOn', text: AI_OPT_IN_COPY.confirm }],
    },
    {
      // Before logging in: the profile's own login is empty, so the row says so.
      id: 'ai-login',
      profile: 'sam',
      actions: [{ click: 'launcher.kiwi' }, { waitFor: 'kiwi.login' }],
      point: { anchor: 'kiwi.login' },
      crop: ['kiwi.account'],
      expect: [{ anchor: 'kiwi.account', text: 'Not logged in' }],
    },
    {
      // The result is the point: the image is now a row under Attached, above the question box.
      id: 'ai-attach',
      profile: 'sam',
      project: 'ln',
      image: 'FtGoJO',
      route: '/manage-images',
      actions: [{ click: 'images.actions' }, { click: 'text:Add to Kiwi' }, { waitFor: 'kiwi.attached' }],
      point: { anchor: 'kiwi.attached', label: 'What Kiwi will read', side: 'top' },
      crop: ['kiwi.attached', 'kiwi.attach'],
      expect: [{ anchor: 'kiwi.attached', text: 'Attached' }],
    },
    {
      // The saved reply to the fixture's question (never a live turn): claims, one per row, with refs.
      id: 'ai-reply',
      profile: 'sam',
      project: 'ln',
      actions: [{ panel: 'kiwi', w: 400, h: 836, y: 60 }, { click: 'launcher.kiwi' }, { waitFor: 'kiwi.turn' },
                { scroll: 'kiwi.turnActions' }],
      point: { anchor: 'kiwi.claimRefs' },
      crop: ['kiwi.turn'],
      masks: ['kiwi.turnMeta'],
      expect: [{ anchor: 'kiwi.turn', text: 'Points at' }, { anchor: 'kiwi.turn', text: 'Save' }],
    },
    {
      // Before choosing: the note is written, so Good and Bad are live.
      id: 'ai-keep',
      profile: 'sam',
      project: 'ln',
      route: '/blackboard',
      actions: [{ click: 'text:What are this image’s channels' }, { click: 'text:Tag outcome' },
                { fill: 'css:.bb-outcome-input', text: 'Matches the microscope settings' }],
      point: { anchor: 'blackboard.good', label: 'Then Good or Bad' },
      crop: ['blackboard.head', 'blackboard.outcome'],
      masks: ['blackboard.updated'],
      expect: [{ anchor: 'blackboard.head', text: 'by Kiwi' }, { anchor: 'blackboard.good', text: 'Good' }],
    },
  ],
}
