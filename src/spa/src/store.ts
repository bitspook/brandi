import createStore from 'zustand';
import { immer } from 'zustand/middleware/immer';
import { apiUrl } from './config';

interface GithubEvent {
  id: string;
  type: string;
  actor: {
    id: number;
    login: string;
    display_login: string;
    url: string;
    avatar_url: string;
  };
  org: {
    id: number;
    login: string;
    url: string;
    avatar_url: string;
  };
  repo: {
    id: number;
    name: string;
    url: string;
  };
  payload: any;
  created_at: string;
  [x: string]: any;
}

interface AppState {
  github: {
    events: GithubEvent[];
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
        const events = await fetch(apiUrl).then((r) => r.json());

        set((state) => {
          state.github.events = events.map((e: any) => e.payload);
        });
      }
    };
  })
);
