import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import listPlugin from '@fullcalendar/list';
import tippy from 'tippy.js';
import { notifyUser } from "./utils"
import { generateColor } from "../utils/color"

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
    const contactEmail = extendedProps.contact_email
    const { assignment_count, max_participants, min_participants, overbook, links } = extendedProps;
    const { show_experiment, show_session, show_location_session, experiment, session, location_session } = links;
    const counterHtml = `<p><strong>Participants: ${assignment_count} / ${max_participants}</strong><br>Overbook: ${overbook}<br>Min. participants: ${min_participants}</p>`;
    const sessionLink = show_session ? `<a href="${session}">Session details</a>` : '';
    const locationSessionLink = show_location_session ? `<a href="${location_session}">Session details</a>` : '';
    const experimentLink = show_experiment ? `<a href="${experiment}">Experiment details</a>` : '';
    var linkList = [];
    if (show_session) {
        linkList.push(sessionLink);
    } else if (show_location_session) {
        linkList.push(locationSessionLink);
    }
    show_experiment ? linkList.push(experimentLink) : null;
    const linksHtml = !linkList.lenght ? `<p>${linkList.join(`<br/>`)}</p>` : '';
    const contactPersonHtml = contactEmail ? `<a href="mailto:${contactEmail}">${contactEmail}</a><br>` : ''
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
                    [...info.el.querySelectorAll('.fc-list-event-title')].map(x => x.innerHTML += `<span>${info.event.extendedProps.location.name}</span>`);
                    tippy(info.el, {
                        arrow: false,
                        appendTo: el,
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
                slotEventOverlap: false
            }).render();
        } catch (error) {
            console.error(error)
        }
    })
}
