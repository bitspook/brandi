import * as s from './styles.module.scss';
import { formatRelative } from 'date-fns';
import { GithubEvent } from '../../store';

export default ({ event }: { event: GithubEvent }) => {
  return (
    <section className={s.event}>
      <header className={s.eventTitle}>
        <img className={s.avatar} src={event.actor.avatar_url} />
        <a href={event.actor.url}>@{event.actor.display_login}</a>
        &nbsp;
        {event.payload.action} to &nbsp;
        <a href={event.repo.url}>{event.repo.name}</a>
        &nbsp; {formatRelative(event.created_at, new Date())}
      </header>
      <article className={s.eventBody}>Some really awesome code.</article>
    </section>
  );
};
