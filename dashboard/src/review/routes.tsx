// Expert review routes (docs/QC_SYSTEM_DESIGN.md 5). The shell mounts `reviewRoute` under AppRoot at /review,
// before the catch-all. It has a place but no top-level tab (shell/modules.ts): the Explorer is unchanged for
// ordinary visitors. ReviewLayout (the campaign data, the profile, the module links) and every view are lazy
// route chunks, so no other module's first load grows; this file is in the entry and imports nothing from the
// review module.
//   /review                 ReviewHome: "Help us break the Explorer", the one primary action, resume, then the optional deeper work
//   /review/profile         ProfileView: the short reviewer profile and the review mode
//   /review/start           CalibrationView: the one-minute calibration before the first case (?next= return path)
//   /review/cases           AssignmentsView: the assigned cases and one self-selected case
//   /review/case/:caseId    CaseReviewView: the blind-first case review (DecisionUseStep, ThreatStep inside)
//   /review/methods         MethodCardsView: the methodology red team
//   /review/framework       FrameworkView: the framework comprehension scenarios
//   /review/break           BreakModelView: "Break the model"
//   /review/summary         CompletionView: the reviewer's completion summary and the optional next steps
//   /review/admin           AdminDashboardView: the internal QC dashboard (not linked from the reviewer pages)
// Anything else under /review goes to /review.

import { Navigate, type RouteObject } from 'react-router';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

export const reviewRoute: RouteObject = {
  path: 'review',
  lazy: view(() => import('./ReviewLayout.tsx')),
  children: [
    { index: true, lazy: view(() => import('./views/ReviewHome.tsx')) },
    { path: 'profile', lazy: view(() => import('./views/ProfileView.tsx')) },
    { path: 'start', lazy: view(() => import('./views/CalibrationView.tsx')) },
    { path: 'cases', lazy: view(() => import('./views/AssignmentsView.tsx')) },
    { path: 'case/:caseId', lazy: view(() => import('./views/CaseReviewView.tsx')) },
    { path: 'methods', lazy: view(() => import('./views/MethodCardsView.tsx')) },
    { path: 'framework', lazy: view(() => import('./views/FrameworkView.tsx')) },
    { path: 'break', lazy: view(() => import('./views/BreakModelView.tsx')) },
    { path: 'summary', lazy: view(() => import('./views/CompletionView.tsx')) },
    { path: 'admin', lazy: view(() => import('./views/AdminDashboardView.tsx')) },
    { path: '*', element: <Navigate to="/review" replace /> },
  ],
};
