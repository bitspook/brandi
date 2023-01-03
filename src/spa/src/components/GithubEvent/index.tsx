import { GithubEvent } from '../../store';
import DeleteEvent from './DeleteEvent';
import GenericEvent from './GenericEvent';

export default ({ event }: { event: GithubEvent }) => {
  switch (event.type) {
    case 'DeleteEvent':
      return <DeleteEvent event={event} />;
    default:
      return <GenericEvent event={event} />;
  }
};
