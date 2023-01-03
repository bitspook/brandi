import * as styles from './styles.module.scss';
import useStore from '../../store';
import { useEffect } from 'react';

export default () => {
  const events = useStore((state) => state.github.events);
  const fetchEvents = useStore((s) => s.fetchGithubEvents);

  useEffect(() => {
    fetchEvents();
  }, []);

  return (
    <div>
      <nav className={styles.topNav}>
        <div className={styles.brand}>Brandi</div>
      </nav>
      <div className={styles.container}>
        <div className={styles.feed}>
          {events.map((event) => (
            <section key={event.id} className={styles.event}>
              <header className={styles.eventTitle}>
                <img className={styles.avatar} src={event.actor.avatar_url} />
                <a href={event.actor.url}>@{event.actor.display_login}</a>
                &nbsp;
                {event.payload.action} to &nbsp;
                <a href={event.repo.url}>{event.repo.name}</a>
                &nbsp; on {event.created_at}
              </header>
              <article className={styles.eventBody}>
                Some really awesome code.
              </article>
            </section>
          ))}
        </div>
      </div>
    </div>
  );
};
