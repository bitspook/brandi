import * as styles from './styles.module.scss';
import useStore from '../../store';
import { useEffect } from 'react';
import GithubEvent from '../../components/GithubEvent';

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
            <GithubEvent event={event} key={event.id} />
          ))}
        </div>
      </div>
    </div>
  );
};
