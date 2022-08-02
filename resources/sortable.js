var observeDOM = (function () {
    var MutationObserver = window.MutationObserver || window.WebKitMutationObserver;

    return function (obj, callback) {
        if (!obj || obj.nodeType !== 1) return;

        if (MutationObserver) {
            // define a new observer
            var mutationObserver = new MutationObserver(callback)

            // have the observer observe foo for changes in children
            mutationObserver.observe(obj, { childList: true, subtree: true })
            return mutationObserver
        }

        // browser support fallback
        else if (window.addEventListener) {
            obj.addEventListener('DOMNodeInserted', callback, false)
            obj.addEventListener('DOMNodeRemoved', callback, false)
        }
    }
})()

var hintClass = "hint"
var activeClass = "active"
var originClass = "origin"

function slist(target) {
    target.classList.add("slist");
    var items = target.children, dragged = null;
    var currentLength = items.length;

    function addListener(i) {
        i.draggable = true;

        i.addEventListener("dragstart", function (e) {
            dragged = this;
            for (let it of items) {
                it.classList.add(it != dragged ? hintClass : originClass);
            }
        });

        i.addEventListener("dragenter", function (e) {
            if (this != dragged) {
                for (let i of items) {
                    i.classList.remove(activeClass)
                }
                this.classList.add(activeClass);
            }
        });

        i.addEventListener("dragend", function () {
            for (let it of items) {
                it.classList.remove(hintClass, originClass, activeClass);
            }
        });

        i.addEventListener("dragover", function (e) {
            e.preventDefault();
        });

        i.addEventListener("drop", function (e) {
            e.preventDefault();
            if (this != dragged) {
                let draggedpos = 0, droppedpos = 0;
                for (let it = 0; it < items.length; it++) {
                    if (dragged == items[it]) { draggedpos = it; }
                    if (this == items[it]) { droppedpos = it; }
                }
                if (draggedpos < droppedpos) {
                    this.parentNode.insertBefore(dragged, this.nextSibling);
                } else {
                    this.parentNode.insertBefore(dragged, this);
                }
            }
            dragged = null;
        });
    }

    for (let i of items) {
        addListener(i);
    }

    observeDOM(target, function (m) {
        if (m[0].target.children.length > currentLength) {
            for (let i of m[0].addedNodes) {
                addListener(i);
            }
            currentLength = m[0].target.children.length;
        } else if (m[0].target.children.length < currentLength) {
            currentLength--;
        }
    });
}

export function initSortable() {
    window.addEventListener("DOMContentLoaded", function () {
        document.querySelectorAll('[data-sortable]').forEach(elm => {
            slist(elm);
        });
    });
}
