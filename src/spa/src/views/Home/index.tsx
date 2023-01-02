import * as styles from './styles.module.scss';
import useStore from '../../store';
import { useEffect } from 'react';

export default () => {
  const events = useStore((state) => state.github.events);
  const fetchEvents = useStore((s) => s.fetchGithubEvents);

  console.log('EVENTS', events);

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
          <section className={styles.event}>
            <header className={styles.eventTitle}>
              @klnegi pushed to ullu
            </header>
            <article className={styles.eventBody}>
              Some really awesome code.
            </article>
          </section>
        </div>
      </div>
    </div>
  );
};
