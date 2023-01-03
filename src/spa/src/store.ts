import createStore from 'zustand';
import { immer } from 'zustand/middleware/immer';
import { apiUrl } from './config';

// From: https://docs.github.com/en/developers/webhooks-and-events/events/github-event-types
export enum GithubEventType {
  CommitCommentEvent = 'CommitCommentEvent',
  CreateEvent = 'CreateEvent',
  DeleteEvent = 'DeleteEvent',
  ForkEvent = 'ForkEvent',
  GollumEvent = 'GollumEvent',
  IssueCommentEvent = 'IssueCommentEvent',
  IssuesEvent = 'IssuesEvent',
  MemberEvent = 'MemberEvent',
  PublicEvent = 'PublicEvent',
  PullRequestEvent = 'PullRequestEvent',
  PullRequestReviewEvent = 'PullRequestReviewEvent',
  PullRequestReviewCommentEvent = 'PullRequestReviewCommentEvent',
  PullRequestReviewThreadEvent = 'PullRequestReviewThreadEvent',
  PushEvent = 'PushEvent',
  ReleaseEvent = 'ReleaseEvent',
  SponsorshipEvent = 'SponsorshipEvent',
  WatchEvent = 'WatchEvent'
}

export interface GithubEvent {
  id: string;
  type: GithubEventType;
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
  created_at: Date;
  [x: string]: any;
}

export interface AppState {
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
          state.github.events = events.map((e: any) => ({
            ...e.payload,
            created_at: new Date(e.payload.created_at)
          }));
        });
      }
    };
  })
);
