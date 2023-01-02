export interface FlashCardPayload {}

export interface VocabCardPayload extends FlashCardPayload {
  word: string;
  meaning: string;
}

export interface VocabExample {
  id: string;
  body: string;
  words: string[];
}
