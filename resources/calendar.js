import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import listPlugin from '@fullcalendar/list';
import { notifyUser } from "./admin/utils"

const notificationId = "calendar-notification";

const viewBreakpoint = 765
const maxHeight = 800

const normalizeSession = (session) => {
    session.end = session.end_
    delete session.end_
    return session
}

const determineView = () => window.innerWidth >= viewBreakpoint ? "dayGridMonth" : "listWeek";

export const initCalendar = () => {
    document.querySelectorAll("[data-calendar]").forEach(el => {
        const { location } = el.dataset
        new Calendar(el, {
            plugins: [dayGridPlugin, timeGridPlugin, listPlugin],
            initialView: 'dayGridMonth',
            firstDay: 1,
            height: maxHeight,
            headerToolbar: {
                left: 'prev,next today',
                center: 'title',
                right: 'dayGridMonth,timeGridWeek,listWeek'
            },
            eventTimeFormat: {
                hour: "numeric",
                minute: "2-digit",
                meridiem: false,
                hour12: false,
            },
            eventSources: [{
                url: `/admin/locations/${location}/sessions`,
                success: e => e.map(e => normalizeSession(e)),
                failure: e => { notifyUser(notificationId, "error", e) }
            }],
            initialView: determineView(),
            windowResize: function () {
                this.changeView(determineView())
            },
        }).render();
    })
}
