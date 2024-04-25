const frameClass = "print-frame";

export const initPrint = () => {
    document.querySelectorAll("[data-print]").forEach((element) => {
        element.addEventListener("click", (ev) => {
            ev.preventDefault();

            const body = document.querySelector("body");
            const currentUrl = window.location.pathname;
            const printUrl = `${currentUrl}/print`
            const printFrame = document.createElement("iframe");

            printFrame.src = printUrl;
            printFrame.style.visibility = "hidden";
            printFrame.style.height = "0";
            printFrame.style.overflow = "hidden";
            printFrame.classList.add(frameClass);

            body.appendChild(printFrame);

            const contentWindow = printFrame.contentWindow;
            contentWindow.addEventListener("load", () => {
                contentWindow.print();
            });
        })
    })

    window.addEventListener("afterprint", () => {
        document.querySelectorAll(`.${frameClass}`).forEach(frame => {
            frame.remove();
        })
    });
}
