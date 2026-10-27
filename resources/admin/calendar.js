import { Calendar } from 'fullcalendar';
import dayGridPlugin from 'fullcalendar/daygrid';
import timeGridPlugin from 'fullcalendar/timegrid';
import listPlugin from 'fullcalendar/list';
import classicThemePlugin from 'fullcalendar/themes/classic';
import tippy from 'tippy.js';
import { notifyUser } from "./utils"
import { generateColor } from "../utils/color"

const notificationId = "calendar-notification";

const viewBreakpoint = 765
const maxHeight = 800

const toLocalTime = date => date.toLocaleTimeString('en',
    {
        timeStyle: 'short',
        hour12: false
    })

const normalizeSession = (session) => {
    const color = generateColor(session.title)
    session.end = session.end_
    delete session.end_
    session.contrastColor = "#363636"
    session.color = color
    session.display = "block"
    return session
}

const isDesktop = () => window.innerWidth >= viewBreakpoint;

const determineView = () => isDesktop() ? "dayGridMonth" : "listWeek";

const tooltipContent = ({ start, end, title, extendedProps }, hideLocation) => {
    const contactEmail = extendedProps.contact_email
    const { assignment_count, max_participants, min_participants, overbook, links } = extendedProps;
    const { experiment, session } = links;
    const counterHtml = `<p><strong>Participants: ${assignment_count} / ${max_participants}</strong><br>Overbook: ${overbook}<br>Min. participants: ${min_participants}</p>`;
    const sessionLink = session && `<a href="${session}">Session details</a>`;
    const experimentLink = experiment && `<a href="${experiment}">Experiment details</a>`;
    var linkList = [sessionLink, experimentLink].filter(Boolean);
    const linksHtml = linkList.length ? `<p>${linkList.join(`<br/>`)}</p>` : '';
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

            const calendar = new Calendar(el, {
                plugins: [dayGridPlugin, timeGridPlugin, listPlugin, classicThemePlugin],
                firstDay: 1,
                height: maxHeight,
                headerToolbar: {
                    left: 'prev,next today',
                    center: 'title',
                    right: 'dayGridMonth,timeGridWeek,listWeek'
                },
                headerToolbarClass: 'calendar-toolbar',
                toolbarSectionClass: 'calendar-toolbar-section',
                toolbarTitleClass: 'calendar-toolbar-title',
                buttonClass: 'calendar-button',
                eventClass: 'calendar-event',
                eventInnerClass: 'calendar-event-inner',
                eventTimeClass: 'calendar-event-time',
                columnEventClass: 'calendar-timegrid-event',
                listDayHeaderClass: info => info.isToday ? 'calendar-list-day-today' : '',
                listDayHeaderInnerClass: 'calendar-list-day-header-inner',
                views: {
                    list: {
                        listItemEventClass: 'calendar-list-event',
                        listItemEventTitleClass: 'calendar-list-event-title',
                    }
                },
                eventTimeFormat: {
                    hour: "numeric",
                    minute: "2-digit",
                    meridiem: false,
                    hour12: false,
                },
                eventDidMount: function (info) {
                    [...info.el.querySelectorAll('.calendar-list-event-title')].forEach(x => { const span = document.createElement('span'); span.textContent = info.event.extendedProps.location.name; x.appendChild(span); });
                    tippy(info.el, {
                        arrow: false,
                        appendTo: el,
                        content: tooltipContent(info.event, hideLocation),
                        allowHTML: true,
                        placement: isDesktop() ? 'right' : 'top',
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
                slotEventOverlap: false
            });
            calendar.render();

            let desktop = isDesktop();
            window.addEventListener('resize', () => {
                if (desktop !== isDesktop()) {
                    desktop = isDesktop();
                    calendar.changeView(determineView());
                }
            });
        } catch (error) {
            console.error(error)
        }
    })
}
