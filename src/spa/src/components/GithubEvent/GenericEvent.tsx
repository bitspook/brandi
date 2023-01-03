import * as s from './styles.module.scss';
import { GithubEvent } from '../../store';
import format from 'date-fns/format';
import formatRelative from 'date-fns/formatRelative';

export default ({ event }: { event: GithubEvent }) => {
  const body = JSON.stringify(event.payload, null, '  ');

  return (
    <section className={s.event}>
      <header>
        <img className={s.avatar} src={event.actor.avatar_url} />

        <div className={s.eventTitle}>
          <div className={s.title}>
            <a href={event.actor.url}>@{event.actor.display_login}</a>
            &nbsp;did something in&nbsp;
            <a href={event.repo.url}>{event.repo.name}</a>
          </div>
          <div className={s.subtitle} title={format(event.created_at, 'PPpp')}>
            {formatRelative(event.created_at, new Date())}
          </div>
        </div>
      </header>

      <article className={s.eventBody} title={body}>
        {body.substring(0, 300)}...
      </article>
    </section>
  );
};
