import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import listPlugin from '@fullcalendar/list';

import { notifyUser, globalErrorMsg } from "./admin/utils"
const notificationId = "calendar-notification";

const parseDate = (str) => new Date(Date.parse(str))

const normalizeSession = (session) => {
    const start = parseDate(session.start);
    session.start = start;
    session.end = start.setSeconds(start.getSeconds() + session.duration)
    return session
}

export const initCalendar = () => {
    document.querySelectorAll("[data-calendar]").forEach(async (el) => {
        const { location } = el.dataset

        const calendar = new Calendar(el, {
            plugins: [dayGridPlugin, timeGridPlugin, listPlugin],

            initialView: 'dayGridMonth',
            headerToolbar: {
                left: 'prev,next today',
                center: 'title',
                right: 'dayGridMonth,timeGridWeek,listWeek'
            },
        });
        try {
            const url = `/admin/locations/${location}/sessions`
            const response = await fetch(url);
            const data = await response.json();
            if (!response.ok) {
                throw (data.message || response.statusText || globalErrorMsg)
            }
            if (response.status < 200 || response.status > 300) {
                notifyUser(notificationId, "error", data.message)
            } else {
                const events = data.map(normalizeSession)
                events.forEach(event => {
                    calendar.addEvent(event)
                })
                calendar.render();
            }
        } catch (error) {
            notifyUser(notificationId, "error", error)
        };

        calendar.render();
    })
}
