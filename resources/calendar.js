import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import listPlugin from '@fullcalendar/list';

import { notifyUser } from "./admin/utils"
const notificationId = "calendar-notification";

const parseDate = (str) => new Date(Date.parse(str))

const normalizeSession = (session) => {
    const start = parseDate(session.start);
    session.start = start;
    session.end = start.setSeconds(start.getSeconds() + session.duration)
    return session
}

export const initCalendar = () => {
    document.querySelectorAll("[data-calendar]").forEach(el => {
        const { location } = el.dataset
        new Calendar(el, {
            plugins: [dayGridPlugin, timeGridPlugin, listPlugin],
            initialView: 'dayGridMonth',
            headerToolbar: {
                left: 'prev,next today',
                center: 'title',
                right: 'dayGridMonth,timeGridWeek,listWeek'
            },
            eventSources: [{
                url: `/admin/locations/${location}/sessions`,
                success: e => e.map(e => normalizeSession(e)),
                failure: e => { notifyUser(notificationId, "error", e) }
            }]
        }).render();
    })
}
