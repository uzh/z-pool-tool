const frameClass = "print-frame";

export const initPrint = () => {
    document.querySelectorAll("[data-print]").forEach((element) => {
        element.addEventListener("click", (ev) => {
            ev.preventDefault();

            const body = document.querySelector("body");
            const currentUrl = window.location.href;
            const printUrl = `${currentUrl}/print`
            const printFrame = document.createElement("iframe");

            printFrame.src = printUrl;
            printFrame.style.visibility = "hidden";
            printFrame.style.height = "0";
            printFrame.style.overflow = "hidden";
            printFrame.classList.add(frameClass);

            body.appendChild(printFrame);
            printFrame.contentWindow.print();
        })
    })
}
