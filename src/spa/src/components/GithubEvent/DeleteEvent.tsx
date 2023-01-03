import * as s from './styles.module.scss';
import { GithubEvent } from '../../store';
import format from 'date-fns/format';
import formatRelative from 'date-fns/formatRelative';
import deleteForverIcon from '../../icons/delete-forever.svg';

export default ({ event }: { event: GithubEvent }) => {
  return (
    <section className={s.event}>
      <header>
        <img className={s.avatar} src={event.actor.avatar_url} />

        <div className={s.eventTitle}>
          <div className={s.title}>
            <a href={event.actor.url}>@{event.actor.display_login}</a>
            &nbsp;deleted a&nbsp;
            {event.payload.ref_type}
            &nbsp;of&nbsp;
            <a href={event.repo.url}>{event.repo.name}</a>
          </div>
          <div className={s.subtitle} title={format(event.created_at, 'PPpp')}>
            {formatRelative(event.created_at, new Date())}
          </div>
        </div>
      </header>

      <article className={s.eventBody}>
        <img className={s.eventIcon} src={deleteForverIcon} />
        {event.payload.ref}
      </article>
    </section>
  );
};
