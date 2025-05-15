import { initCalendar } from './admin/calendar'
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initDuplicateSessionForm } from "./admin/duplicateSessions.js";
import { initFilterForm } from "./admin/filter.js"
import { initPrint } from "./admin/print"
import { initHtmxSearch } from "./admin/search.js"
import { initAssignmentListMessaging } from "./admin/assignmentListMessaging.js";

initCalendar();
initCopyClipboard();
initDuplicateSessionForm();
initFilterForm();
initPrint();
initHtmxSearch();

window['pool-tool'] = {
    initAssignmentListMessaging
};
