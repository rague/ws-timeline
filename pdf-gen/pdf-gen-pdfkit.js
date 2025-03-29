// const PDFDocument = require("pdfkit");
// const blobStream = require("blob-stream");


function sanitize(s) {
  return s.replace(/[^a-z0-9]/gi, '_');
}

var pdfurl;

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

function download() {
  if (!blob) return;
  var url = window.URL.createObjectURL(blob);
  a.href = url;
  a.download = title;
  a.click();
  window.URL.revokeObjectURL(url);
}


function generatePDF(pages, t) {

  let doc = new PDFDocument({ autoFirstPage: false });
  doc.info.title = t;
  title = t;
  let stream = doc.pipe(blobStream());

  pages.forEach((page) => {
    this._drawPdfPage(page, doc);
  });

  doc.end();

  stream.on('finish', () => {
    blob = stream.toBlob("application/pdf");
    url = stream.toBlobURL('application/pdf');
    const iframe = document.getElementById("iframe");
    iframe.src = url;
    pdfurl = url;
    if (previewed) document.getElementById("preview").src = pdfurl;
  });

}

function _drawPdfPage(page, doc) {
  let font = 'Helvetica', fstyle = '', fweight = '', talign = 'left';
  let mode = 1;

  doc.addPage({
    size: [page.width * MM_PT, page.height * MM_PT],
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
        if (PDF_STYLES.indexOf(prop) !== -1) {
          doc[prop](item[prop]);
        } else {
          switch (prop) {
            case 'lineWidth':
              doc.lineWidth(item[prop] * MM_PT);
              break;
            case 'fontSize':
              doc.fontSize(item[prop] * MM_PT);
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
          }
        }
      }
    }
    // if (fchange) {
    //   doc.font(font + (fweight + fstyle ? '-' + fweight + fstyle : ''));
    // }

    // styles applied


    switch (item.type) {
      case 'clip':
        doc.save();
        doc.rect(pts[0], pts[1], pts[2], pts[3]);
        doc.clip();
        break;

      case 'restore':
        doc.restore();
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
          doc.closePath();
        }
        drawpath = true;
        break;

      case 'rect':
        // ctx.beginPath();

        if (item.radius) {
          doc.roundedRect(pts[0], pts[1], pts[2], pts[3], parseInt(item.radius));
        }
        else {
          doc.rect(pts[0], pts[1], pts[2], pts[3]);
        }


        switch (mode) {
          case 1:
            doc.stroke();
            break;
          case 2:
            doc.fill();
            break;
          case 3:
            doc.fillAndStroke();
            break;
        }


        if (item.clip) {
          doc.save();
          if (item.radius) {
            doc.roundedRect(pts[0], pts[1], pts[2], pts[3], parseInt(item.radius));
          }
          else {
            doc.rect(pts[0], pts[1], pts[2], pts[3]);
          }
          doc.clip();
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
          width: max * MM_PT * (item.multiline || talign == "right" ? 1 : 2),
          lineBreak: item.multiline !== undefined ? item.multiline : false,
          // width: max * MM_PT,
          // lineBreak: true,
          height: item.multiline ? height : 0
        };

        let theight = doc.heightOfString(text, options);
        // let theight = height;



        // if(max) {
        doc.text(text, (item.x - delta) * MM_PT, item.y * MM_PT,
          // doc.text(text, (item.x - delta) * MM_PT, item.y * MM_PT + Math.max((height - theight) / 2, 0),
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
          doc.fillAndStroke();
          break;
      }


    }
  });
}
