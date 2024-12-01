
function sanitize(s) {
  return s.replace(/[^a-z0-9]/gi, '_');
}


/*
  document vectoriel
  - imprimable via export pdf
  - affichable à l'écran
  - zoomable
  - multipage

fonts: Times (reg, bold, italic, bold italic),
 Helvetica (in regular, oblique, bold and bold oblique),
 Courier (in regular, oblique, bold and bold oblique)

  format :

  [ // array of pages
    { // page
      width: <width in mm>,
      height: <height in mm>,
      data: [ // array of instructions
        {
          type: 'style',
          stroke: ,
          fill: ,
          font: ,
          fontSize: ,
          textAlign: ,
          ...

        }, {
          type: 'line',
          points: [x1, y1, x2, y2]
        },{
          type: 'polyline',
          closed: bool,
          points: [x1, y1, ...]
        },{
          type: 'rect',
          mode: 1 || 2 || 3, // 1 stroke 2 fill 3 stroke & fill
          radius: <radius in mm> | [topleft, topright, bottomright, bottomleft],
          points: [x1, y1, x2, y2]
        },{
          type: 'text',
          text: <bla>,
          x:
          y:
          maxwidth:
        }
      ]
    }
  ]

 */




// 25.4mm = 72pt
// 1 pt = .353 mm
// 1 px = 1mm
// 1 fpx = 1 pt
// 1 fpx = .353 mm
// 1 fpx = .353 px
//


const PT_MM = 25.4 / 72;
const MM_PT = 72 / 25.4;

function pt2mm(pt) {
  return pt * PT_MM;
}



const STYLES = ['fillStyle', 'strokeStyle', 'shadowColor', 'shadowBlur', 'shadowOffsetX',
  'shadowOffsetY', 'lineCap', 'lineJoin', 'miterLimit', 'globalAlpha', 'textAlign'];

const PDF_STYLES = ['lineCap', 'lineJoin', 'miterLimit', 'fillColor', 'strokeColor', 'fillOpacity', 'strokeOpacity', 'opacity'];




let a;
let title;

window.addEventListener('load', (event) => {
  a = document.createElement("a");
  document.body.appendChild(a);
  a.style = "display: none";
})


let blob;
let url;

function download() {
  if (!blob) return;
  a.href = url;
  a.download = title;
  a.click();
  // window.URL.revokeObjectURL(url);
}


function generatePDF(pages, t) {

  let doc = new jspdf.jsPDF({ unit: "pt" });
  title = t;

  pages.forEach((page) => {
    this._drawPdfPage(page, doc);
  });



  blob = doc.output('blob') //new Blob(doc.output('blob'), { type: 'application/pdf' });
  url = window.URL.createObjectURL(blob);
  const iframe = document.getElementById("iframe");
  iframe.src = url;

  console.log("BLA");

}

function _drawPdfPage(page, doc) {
  let font = 'Helvetica', fstyle = '', fweight = '', talign = 'left', fsize;
  let mode = 1, fillColor = { r: 200, g: 200, b: 200 }, fillOpacity = 1;

  doc.addPage({
    format: [page.width * MM_PT, page.height * MM_PT],
    margins: 0
  });



  page.data.forEach(function (item, idx) {
    // console.log("pdf draw", idx, item);
    // apply styles
    let fchange = false;
    let drawpath = false;
    let pts = item.points;

    if (pts) {
      pts = pts.map((pt) => { return pt * MM_PT; });
    }


    for (let prop in item) {
      if (item.hasOwnProperty(prop)) {
        // if (PDF_STYLES.indexOf(prop) !== -1) {
        //   doc[prop](item[prop]);
        // } else {
        switch (prop) {
          case 'lineWidth':
            doc.setLineWidth(item[prop] * MM_PT);
            break;
          case 'fontSize':
            doc.setFontSize(item[prop] * MM_PT);
            fsize = item[prop] * MM_PT;
            break;
          case 'font':
            font = item[prop];
            fchange = true;
            break;
          case 'fontWeight':
            fweight = item[prop];
            fchange = true;
            break;
          case 'fontStyle':
            fstyle = item[prop];
            fchange = true;
            break;
          case 'mode':
            mode = item.mode;
            break;
          case 'textAlign':
            talign = item.textAlign;
            break;
          case 'fillColor':
            let fillColorStr = item[prop];
            if (fillColorStr.length == 4) {
              fillColor.r = parseInt("0x" + fillColorStr[1] + fillColorStr[1]);
              fillColor.g = parseInt("0x" + fillColorStr[2] + fillColorStr[2]);
              fillColor.b = parseInt("0x" + fillColorStr[3] + fillColorStr[3]);
            } else {
              fillColor.r = parseInt("0x" + fillColorStr[1] + fillColorStr[2]);
              fillColor.g = parseInt("0x" + fillColorStr[3] + fillColorStr[4]);
              fillColor.b = parseInt("0x" + fillColorStr[5] + fillColorStr[6]);
            }
            console.log("FillColor", item[prop], fillColor, fillOpacity);
            doc.setFillColor(fillColor.r, fillColor.g, fillColor.b);
            // doc.setFillColor(fillColor.r, fillColor.g, fillColor.b, fillOpacity);
            // doc.setFillColor("#aaaaaa");
            break;
          case "fillOpacity":
            fillOpacity = item[prop];
            // doc.setFillColor(fillColor.r, fillColor.g, fillColor.b, fillOpacity);
            break;
        }
        // }
      }
    }
    // if (fchange) {
    //   doc.font(font + (fweight + fstyle ? '-' + fweight + fstyle : ''));
    // }

    // styles applied


    switch (item.type) {
      case 'clip':
        doc.saveGraphicsState();
        doc.rect(pts[0], pts[1], pts[2], pts[3], null);
        doc.clip();
        doc.discardPath();
        break;

      case 'restore':
        doc.restoreGraphicsState();
        break;

      case 'line':
        // ctx.beginPath();
        doc.moveTo(pts[0], pts[1]);
        doc.lineTo(pts[2], pts[3]);
        doc.stroke();
        break;

      case 'polyline':
        // ctx.beginPath();
        doc.moveTo(pts[0], pts[1]);
        let j, len = pts.length;
        for (j = 0; j < len; j += 2) {
          doc.lineTo(pts[j], pts[j + 1]);
        }
        if (item.closed) {
          doc.close();
        }
        drawpath = true;
        break;

      case 'rect':
        // ctx.beginPath();

        if (item.radius) {
          doc.roundedRect(pts[0], pts[1], pts[2], pts[3], parseInt(item.radius), parseInt(item.radius), null);
        }
        else {
          doc.rect(pts[0], pts[1], pts[2], pts[3], null);
        }


        switch (mode) {
          case 1:
            doc.stroke();
            break;
          case 2:
            doc.fill();
            break;
          case 3:
            doc.fillStroke();
            break;
        }


        if (item.clip) {
          doc.saveGraphicsState();
          if (item.radius) {
            doc.roundedRect(pts[0], pts[1], pts[2], pts[3], parseInt(item.radius), parseInt(item.radius), null);
          }
          else {
            doc.rect(pts[0], pts[1], pts[2], pts[3], null);
          }
          doc.clip();
          doc.discardPath();
        }
        break;

      case 'text':
        // ctx.beginPath();
        let text = item.text;
        let max = item.maxWidth || 10000;
        let delta = 0;
        let height = (item.maxHeight === undefined ? 0 : item.maxHeight) * MM_PT;

        if (talign === 'center' && item.maxWidth === undefined) {
          delta = max / 2;
        } else if (talign === 'right' && item.maxWidth === undefined) {
          delta = max;
        }

        let options = {
          align: talign,
          width: max * MM_PT * (item.multiline ? 1 : 0)
          // height: item.multiline ? height : 0
        };

        // let theight = doc.heightOfString(text, options);
        // let theight = height;



        // if(max) {
        // doc.text(text, (item.x - delta) * MM_PT, item.y * MM_PT + Math.max((height - theight) / 2, 0),
        doc.text(text + "", (item.x - delta) * MM_PT, item.y * MM_PT + fsize,
          options);
        // } else {
        // doc.text(text, item.x*MM_PT, item.y*MM_PT);
        // }
        break;
    }

    if (drawpath) {
      switch (mode) {
        case 1:
          doc.stroke();
          break;
        case 2:
          doc.fill();
          break;
        case 3:
          doc.fillStroke();
          break;
      }


    }
  });
}
