import { VocabCardPayload } from '../types';

const VOCAB_CARDS: VocabCard[] = [
  {
    word: 'sehr',
    meaning: 'very'
  }
];

const Examples: VocabExample[] = [
  { id: '1', body: 'Der Hund ist sehr gut', words: ['sehr', 'hund'] },
  { id: '2', body: 'Die tasche ist sehr tuer', words: ['sehr', 'tuer'] },
  { id: '3', body: 'Das spitzer ist sehr billig', words: ['sehr', 'billig'] }
];

export const nextCardPayload = async (): Promise<VocabCardPayload> => {
  return VOCAB_CARDS[0];
};
