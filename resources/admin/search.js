const configSearch = (e) => {
    e.detail.parameters["focus"] = e.detail.triggeringEvent.srcElement?.name;
}
export const initHtmxSearch = () => {
    [...document.querySelectorAll(".hx-search")].forEach(elt => {
        elt.addEventListener('htmx:configRequest', (e) => configSearch(e))
        elt.addEventListener('htmx:afterSwap', ({ detail }) => {
            if (detail.requestConfig.parameters.focus === "search") {
                const input = detail.elt.querySelector(`[name="${detail.requestConfig.parameters.focus}"]`);
                input.setSelectionRange(input.value.length, input.value.length);
                input.focus();
            }
        })
    })
}
