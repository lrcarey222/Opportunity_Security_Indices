// Keeps a failing view from taking the chrome down; resets on navigation (moved from App.tsx).
// AppRoot wraps every route in one; StrategicLayout adds its own inside the Strategic chrome so the
// status strip and the dials survive a view error, as before.

import { Component, type ReactNode } from 'react';
import { Notice } from '../components/ui.tsx';

export class ViewErrorBoundary extends Component<{ children: ReactNode; resetKey: string }, { error: Error | null }> {
  state: { error: Error | null } = { error: null };
  static getDerivedStateFromError(error: Error) {
    return { error };
  }
  componentDidUpdate(prev: { resetKey: string }) {
    if (prev.resetKey !== this.props.resetKey && this.state.error) this.setState({ error: null });
  }
  componentDidCatch(error: Error) {
    console.error(error);
  }
  render() {
    if (this.state.error) {
      return (
        <div className="mx-auto max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
          <Notice kind="error" title="This view failed to render">
            {this.state.error.message}. Try another setting or reload the page.
          </Notice>
        </div>
      );
    }
    return this.props.children;
  }
}
