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
    return session
}

export const initCalendar = () => {
    document.querySelectorAll("[data-calendar]").forEach(el => {
        const { location } = el.dataset
        new Calendar(el, {
            plugins: [dayGridPlugin, timeGridPlugin, listPlugin],
            initialView: 'dayGridMonth',
            firstDay: 1,
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
        }).render();
    })
}
