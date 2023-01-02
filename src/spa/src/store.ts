import createStore from 'zustand';
import { immer } from 'zustand/middleware/immer';

interface AppState {
  github: {
    events: Array<number>;
  };
  fetchGithubEvents: () => Promise<void>;
}

export default createStore<AppState>()(
  immer((set) => {
    return {
      github: {
        events: []
      },
      fetchGithubEvents: async () => {
        set((state) => {
          state.github.events = [1, 2, 3];
        });
      }
    };
  })
);
