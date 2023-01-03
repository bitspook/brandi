import * as s from './styles.module.scss';
import { GithubEvent } from '../../store';
import format from 'date-fns/format';
import formatRelative from 'date-fns/formatRelative';

export default ({ event }: { event: GithubEvent }) => {
  return (
    <section className={s.event}>
      <img className={s.avatar} src={event.actor.avatar_url} />

      <header className={s.eventTitle}>
        <div className={s.title}>
          <a href={event.actor.url}>@{event.actor.display_login}</a>
          &nbsp;deleted a&nbsp;
          <a href={`https://github.com/${event.payload.ref}`}>
            {event.payload.ref_type}
          </a>
          &nbsp;of&nbsp;
          <a href={event.repo.url}>{event.repo.name}</a>
        </div>
        <div className={s.subtitle} title={format(event.created_at, 'PPpp')}>
          {formatRelative(event.created_at, new Date())}
        </div>
      </header>
    </section>
  );
};
