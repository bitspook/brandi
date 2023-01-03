import { GithubEvent } from '../../store';
import DeleteEvent from './DeleteEvent';

export default ({ event }: { event: GithubEvent }) => {
  switch (event.type) {
    case 'DeleteEvent':
      return <DeleteEvent event={event} />;
    default:
      return null;
  }
};
