import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import listPlugin from '@fullcalendar/list';
import tippy from 'tippy.js';
import { notifyUser } from "./admin/utils"
import { generateColor } from "./utils/color"

const notificationId = "calendar-notification";

const viewBreakpoint = 765
const maxHeight = 800

const toLocalTime = date => date.toLocaleTimeString('en',
    {
        timeStyle: 'short',
        hour12: false,
        timeZone: 'UTC'
    })

const normalizeSession = (session) => {
    const color = generateColor(session.title)
    session.end = session.end_
    delete session.end_
    session.textColor = "#363636"
    session.backgroundColor = `${color}${Math.floor(0.2 * 255).toString(16)}`
    session.borderColor = color
    session.display = "block"
    return session
}

const determineView = () => window.innerWidth >= viewBreakpoint ? "dayGridMonth" : "listWeek";

const tooltipContent = ({ _instance, _def }, hideLocation) => {
    const { start, end } = _instance.range;
    const { title, extendedProps } = _def;
    const contactPerson = extendedProps.contact_person
    console.log(extendedProps)
    const { assignment_count, experiment_url, session_url, max_participants, min_participants, overbook } = extendedProps;
    const counterHtml = `<p><strong>Participants: ${assignment_count} / ${max_participants}</strong><br>Overbook: ${overbook}<br>Min. participants: ${min_participants}</p>`
    const linksHtml = `<p>
        <a href="${experiment_url}">Experiment details</a><br>
        <a href="${session_url}">Session details</a>
    </p>`
    const contactPersonHtml = contactPerson ? `<a href="mailto:${contactPerson.email}">${contactPerson.name}</a><br>` : ''
    const header = `<div class="card-header">${title}</div>`
    const body = `<div class="card-body">
        <p>${hideLocation ? "" : `${extendedProps.location.name}<br>`}
            ${toLocalTime(start)} - ${toLocalTime(end)}
        </p>
        ${extendedProps.description ? `<br>${extendedProps.description}` : ""}
        ${contactPersonHtml}
        ${counterHtml}
        ${linksHtml}
        </div>`
    const arrow = `<svg class="arrow" viewBox="0 0 460.5 531.74"><polygon points="460,530.874 1,265.87 460,0.866 "/></svg>`
    return `<div class="fc-tooltip">${arrow}<div class="card">${header}${body}</div></div>`
}

export const initCalendar = () => {
    document.querySelectorAll("[data-calendar]").forEach(el => {
        try {
            const { calendar: url, hideLocation } = el.dataset


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
                eventDidMount: function (info) {
                    tippy(info.el, {
                        arrow: false,
                        content: tooltipContent(info.event, hideLocation),
                        allowHTML: true,
                        placement: 'right',
                        delay: [200, 0],
                        interactive: true,
                        trigger: 'click',
                    });
                },
                eventSources: [{
                    url: url,
                    success: e => e.map(e => normalizeSession(e)),
                    failure: e => { notifyUser(notificationId, "error", e) }
                }],
                initialView: determineView(),
                windowResize: function () {
                    this.changeView(determineView())
                },
            }).render();
        } catch (error) {
            console.error(error)
        }
    })
}
