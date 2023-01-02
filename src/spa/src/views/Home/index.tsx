import * as styles from './styles.module.scss';

export default () => {
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
