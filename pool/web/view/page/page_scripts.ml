let sortable_js =
  {js|
      function slist (target) {
        target.classList.add("slist");
        var items = target.children, current = null;
        for (let i of items) {
          i.draggable = true;
          i.addEventListener("dragstart", function (e) {
            current = this;
            for (let it of items) {
              if (it != current) { it.classList.add("hint"); }
            }
          });
          i.addEventListener("dragenter", function (e) {
            if (this != current) { this.classList.add("active"); }
          });
          i.addEventListener("dragleave", function () {
            this.classList.remove("active");
          });
          i.addEventListener("dragend", function () {
            for (let it of items) {
              it.classList.remove("hint");
              it.classList.remove("active");
            }
          });
          i.addEventListener("dragover", function (e) {
            e.preventDefault();
          });
          i.addEventListener("drop", function (e) {
            e.preventDefault();
            if (this != current) {
              let currentpos = 0, droppedpos = 0;
              for (let it=0; it<items.length; it++) {
                if (current == items[it]) { currentpos = it; }
                if (this == items[it]) { droppedpos = it; }
              }
              if (currentpos < droppedpos) {
                this.parentNode.insertBefore(current, this.nextSibling);
              } else {
                this.parentNode.insertBefore(current, this);
              }
            }
          });
        }
      }

      window.addEventListener("DOMContentLoaded", function(){
        document.querySelectorAll('[data-sortable]').forEach(elm => {
          slist(elm);
        });
      });
  |js}
;;
