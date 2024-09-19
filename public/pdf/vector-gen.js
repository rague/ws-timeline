

/*
data

line
{ label: String
, sections: []}

section
{ label : [String]
, start : Int 
, end : Int
, color : String

}

config
{ title : String
, fromDate : Date
, toDate : Date
, vpages : Int
, hpages : Int
, groupsFontSize : Float // points
, tasksFontSize : Float // points
, hoursFontSize : Float // points
, mode : 0 | 1 // Horiz, Vert
, orientation : 0 | 1 // paysage, portrait
, align : 'left' | 'center' | right
, multiline : Bool
, papersize : 'A4' | 'A3' | 'AZ'
}

default 
  vpages: 1,
  hpages: 1,
  groupsFontSize: 11, // points
  tasksFontSize: 8, // points
  hoursFontSize: 6.5, // points
  orientation: 1,
  multiline: true,
  align: 'left',
  mode: 0,
  title: 'TITLE',
  title: '',
 papersize: 'A4',


Values
  alignments: [
    {value: 'left', title: 'à gauche'},
    {value: 'center', title: 'centré'},
    {value: 'right', title: 'à droite'}
  ],

  orientations: [
    {type: 0, title: 'paysage'},
    {type: 1, title: 'protrait'}
  ],

  modes: [
    {type: 0, title: 'horizontal'},
    {type: 1, title: 'vertical'}
  ],

  papersizes: [
    {title: 'A4', width: 210, height: 297},
    {title: 'A3', width: 297, height: 420},
    {title: 'A2', width: 420, height: 594},
  ],


*/




const TITLE_HEIGHT = 10;

// mode horiz
const COL_WIDTH = 50;
const AXIS_HEIGHT = 10;

// mode vertical
const COL_HEIGHT = 10;
const AXIS_WIDTH = 25;

const GRID_UNIT = 5;

const GRIDS = [ // unit : fraction of 1 hour
    {
        unit: 1 / 12, divs: [
            { delta: 5, unit: 'm' },
            { delta: 1, unit: 'h' },
            { delta: 1, unit: 'd' }]
    },
    {
        unit: 1 / 6, divs: [
            { delta: 15, unit: 'm' },
            { delta: 1, unit: 'h' },
            { delta: 1, unit: 'd' }]
    },
    {
        unit: 1 / 2, divs: [
            { delta: 30, unit: 'm' },
            { delta: 1, unit: 'h' },
            { delta: 1, unit: 'd' }]
    },
    {
        unit: 1, divs: [
            { delta: 1, unit: 'h' },
            { delta: 6, unit: 'h' },
            { delta: 1, unit: 'd' }]
    },
    {
        unit: 3, divs: [
            { delta: 2, unit: 'h' },
            { delta: 12, unit: 'h' },
            { delta: 1, unit: 'd' }]
    },
    {
        unit: 6, divs: [
            { delta: 12, unit: 'h' },
            { delta: 1, unit: 'd' },
            { delta: 1, unit: 'M' }]
    },
    {
        unit: 12, divs: [
            { delta: 1, unit: 'd' },
            { delta: 1, unit: 'w' },
            { delta: 1, unit: 'M' }]
    },
    {
        unit: 48, divs: [
            { delta: 1, unit: 'w' },
            { delta: 1, unit: 'M' },
            { delta: 1, unit: 'y' }]
    }
];

const COLORS = {
    blue: '#5AF',
    pink: '#F6B',
    red: '#F65',
    purple: '#B6F',
    green: '#4F4',
    yellow: '#FE0',
    grey: '#EEE',
    cyan: '#4FF',
    orange: '#F92',

    error: '#F00',
    problem: '#FF0',
    warning: '#F90',
    ok: '#0F0'
};

const PAPERSIZES = [
    { title: 'A5', width: 148, height: 210 },
    { title: 'A4', width: 210, height: 297 },
    { title: 'A3', width: 297, height: 420 },
    { title: 'A2', width: 420, height: 594 },
    { title: 'A2', width: 594, height: 841 },
    { title: 'A1', width: 841, height: 1189 }
]

function findColor(string) {
    let a = string.split(" ");
    let j, len = a.length;

    if(string.startsWith("#"))
        return string;

    for (j = 0; j < len; j++) {
        let col = COLORS[a[j]];
        if (col) {
            return col;
        }
    }

    return '#fff';

}

const GRID_WEIGHT = [0.05, 0.2, 0.3];

const GRID_FORMATS = {
    m: ':mm',
    h: 'H[h]',
    d: 'dd D',
    w: 'WW',
    M: 'MMM',
    y: 'YYYY',
};

function gridFor(from, to, width) {
    let duration = to - from;
    let unit = duration / width / 3600000 * GRID_UNIT;
    let grid = GRIDS.find(function (g) {
        return unit < g.unit;
    });

    if (grid === undefined) {
        grid = GRIDS[GRIDS.length - 1];
    }
    return grid;
}

function prepareGrid(from, to, width) {
    let out = [];
    let duration = to - from;

    let grid = gridFor(from, to, width);

   

    let fac = width / duration;


    grid.divs.forEach(function (div, idx) {
        let start = moment(from).startOf(div.unit);
        let dy = idx;
        let data = [];
        out.push({
            lineWidth: GRID_WEIGHT[idx],
            start: from,
            fac,
            dy,
            data
        });
        while (start.valueOf() <= to) {
            let x = (start.valueOf() - from) * fac;
            let text = start.format(GRID_FORMATS[div.unit]);

            start.add(div.delta, div.unit);
            let next = (start.valueOf() - from) * fac;

            if (x >= 0) {
                data.push({
                    type: 'line',
                    x: x,
                });
            }

            if (x >= 0 || (next - 0) > 20) {
                data.push({
                    type: 'text',
                    x: Math.max(0, x) + 0.5,
                    text: text
                });
            }

        }
    });

    return out;
}

function drawGrid(mode, prep, from, to, origin, top, bottom) {
    let out = [];
    let minx = (from - prep.start) * prep.fac;
    let maxx = (to - prep.start) * prep.fac;
    let delta = (prep.start - from) * prep.fac + origin;
    let swap;

    out.push({ lineWidth: prep.lineWidth });
    prep.data.forEach(function (item) {
        let clone = Object.assign({}, item);
        let clonex = clone.x;
        let dy;
        if (clonex >= minx && clonex <= maxx) {
            clonex += delta;
            dy = mode === 0 ? (-(prep.dy * 3 + 3)) : (-prep.dy * 8 - 8);
            if (clone.type === 'text') {
                if (mode === 0) {
                    clone.x = clonex;
                    clone.y = dy + top;
                } else {
                    clone.x = dy + top;
                    clone.y = clonex;
                }
                clone.fontSize = 2.5;
            } else {
                clone.points = mode === 0 ?
                    [clonex, top + dy, clonex, bottom] :
                    [dy + top, clonex, bottom, clonex]
                    ;
            }
            out.push(clone);
        }

    });

    return out;
}


function filled(count, value) {
    let a = [], j;
    for (j = 0; j < count; j++) { a[j] = Object.assign({}, value); }
    return a;
}

function headreplace(list, value) {
    let a = Array.from(list);
    a[0] = value;
    return a;
}

function addSection(lines, s, idx) {
    let a = lines[idx] || [];
    lines[idx] = a;

    a.push(s);
}

function unwrap(line, isEven) {
    let r = 1, last = [], j, k, sections_len, section, last_section = null, sections = line.sections;
    let out = [[]];

    sections.sort(function (a, b) {
        return a.start - b.start;
    });

    sections_len = sections.length;

    for (j = 0; j < sections_len; j++) {
        section = sections[j];


        k = 0;
        if (last_section && (section.start < last_section.end)) {

            do {
                k++;
            } while ((k !== r) && (section.start < last[k].end));
            if (k === r) { r++; }
        }
        addSection(out, section, k);
        last[k] = section;
        if (k === 0) { last_section = section; }
    }


    return out.map(function (s, i) {
        let o = { sections: s };
        if (i === 0) {
            o.label = line.label;
            o.isNewActivity = true;
            o.lines = out.length;
            o.isEven = isEven;
        }
        return o;
    });
}

function layout(lines) {
    let out = [];

    lines.forEach(function (line, i) {
        out.push(...(unwrap(line, i % 2)));
    });


    return out;
}
/*
No intersect :
A a B b => a <= B
B b A a => b <= A
rule :
a <= B || b <= A
*/
function intersect(a, b, A, B) {
    return (a <= B || b <= A);
}




function generate(rawlines, config) {
    let lines = layout(rawlines);
    let hpages = config.hpages, vpages = config.vpages;
    let from = config.fromDate.valueOf(),
        to = config.toDate.valueOf();
    let linecount = lines.length;
    let orient = config.orientation;
    let align = config.align;
    let multiline = config.multiline;
    let marginleft = 15, margintop = 15, marginright = 15, marginbottom = 15;
    let papersize = PAPERSIZES.find(ps => ps.title === config.papersize);
    let fullpagew = orient === 1 ? papersize.width : papersize.height;
    let fullpageh = orient === 1 ? papersize.height : papersize.width;
    let mode = config.mode;

  

    let pagew = fullpagew - marginleft - marginright;
    let pageh = fullpageh - margintop - marginbottom;

    if (mode === 0) {
        pageh -= AXIS_HEIGHT;
    } else {
        pagew -= AXIS_WIDTH;
    }


    let width = hpages * pagew, height = vpages * pageh;

    height -= TITLE_HEIGHT; // title

    if (mode === 0) {
        width -= COL_WIDTH; // activities
    } else {
        height -= COL_HEIGHT; // activities
    }



    function calcMeanLine(pages, pagesize, totsize, count, first, max) {
        let meanline = Math.min(totsize / count, max);
        while (((pages - 1) * Math.floor(pagesize / meanline) + Math.floor(first / meanline)) < count) {
            meanline -= 1;
        }

        return meanline;
    }


    function calcPageInfos(pages, tot, cell, base, mode) {
        let pageinfos = headreplace(
            filled(pages, {
                linecount: Math.floor(tot / cell),
                base: base,
            }),
            mode === 0 ?
                { linecount: Math.floor((tot - TITLE_HEIGHT) / cell), base: base + TITLE_HEIGHT } :
                { linecount: Math.floor(tot / cell), base: base + TITLE_HEIGHT }
        );
        let j, line = 0;
        for (j = 0; j < pages; j++) {
            pageinfos[j].lines = lines.slice(line, line + pageinfos[j].linecount);
            line += pageinfos[j].linecount;
        }

        return pageinfos;
    }

    function calcWidths(pages, size, first) {
        return headreplace(new Array(pages).fill(size), first);
    }

    let meanlineh;
    let pageinfos;
    let widths;
    let axis;
    let msu;


    let grid = gridFor(from, to, mode === 0 ? width : height);
    if(grid.unit >= 6) {
        let d = new Date(from);
        d.setHours(0);
        from = d.valueOf();
    } else if(grid.unit === 3) {
        let d = new Date(from);
        d.setHours(d.getHours() - (d.getHours() % 2));
        from = d.valueOf();
    }

    if (mode === 0) {
        meanlineh = calcMeanLine(vpages, pageh, height, linecount, pageh - TITLE_HEIGHT, 6 * pt2mm(config.groupsFontSize));
        pageinfos = calcPageInfos(vpages, pageh, meanlineh, margintop + AXIS_HEIGHT, mode);
        widths = calcWidths(hpages, pagew, pagew - COL_WIDTH);

        axis = prepareGrid(from, to, width);
        msu = width / (to - from);
    } else {
        meanlineh = calcMeanLine(hpages, pagew, width, linecount, pagew, 120);
        pageinfos = calcPageInfos(hpages, pagew, meanlineh, margintop, mode);
        widths = calcWidths(vpages, pageh, pageh - COL_HEIGHT - TITLE_HEIGHT);
        axis = prepareGrid(from, to, height);
        msu = height / (to - from);
    }



    let gfsize = Math.min(pt2mm(config.groupsFontSize), meanlineh - 1);
    let tfsize = Math.min(pt2mm(config.tasksFontSize), meanlineh - 1);
    let hfsize = Math.min(pt2mm(config.hoursFontSize), meanlineh - 1);



    let pages = [];

    let self = this;

    pageinfos.forEach(function (info, infoidx) {
        let f = from, t;

        widths.forEach(function (psize, hidx) {
            let data = [];
            let clipped = [];

            let page = {
                width: fullpagew,
                height: fullpageh,
                data
            };
            pages.push(page);
            t = f + (psize / msu);
            let origin = mode === 0 ? (fullpagew - marginright - psize) : (fullpageh - marginbottom - psize);


            data.push({ fontSize: gfsize, fontWeight: '' });

            info.lines.forEach(function (line, idx) {
                if (line.isNewActivity) {

                    if (line.isEven) {
                        data.push({
                            type: 'rect',
                            mode: 2,
                            fillColor: '#eaeaea',
                            points: mode === 0 ?
                                [marginleft, idx * meanlineh + info.base, pagew, meanlineh * line.lines] :
                                [idx * meanlineh + marginleft + AXIS_WIDTH, margintop + (TITLE_HEIGHT * (hidx === 0)), meanlineh * line.lines, fullpageh - (TITLE_HEIGHT * (hidx === 0)) - marginbottom - margintop]
                        });
                    }
                    
                    if (hidx === 0) {
                        data.push({
                            type: 'text',
                            fillColor: '#000',
                            x: mode === 0 ? (marginleft + 1) : (idx * meanlineh + marginleft + AXIS_WIDTH + 1),
                            y: mode === 0 ? (idx * meanlineh + info.base + 1) : (margintop + TITLE_HEIGHT + 1),
                            text: typeof line.label === 'string' ? line.label : line.label.join('\n'),
                            maxWidth: mode === 0 ? COL_WIDTH : meanlineh,
                            maxHeight: mode === 0 ? meanlineh : COL_WIDTH,
                        });
                    }

                }

                line.sections.forEach(function (section) {
                    if (intersect(section.start, section.end, f, t)) {
                        let sl;
                        let sw;
                        let st;
                        let sh;

                        if (mode === 0) {
                            sl = origin + ((section.start - f) * msu);
                            sw = (section.end - section.start) * msu;
                            st = idx * meanlineh + info.base + 1;
                            sh = meanlineh - 2;
                        } else {
                            sl = idx * meanlineh + marginleft + AXIS_WIDTH + 1;
                            sw = meanlineh - 2;
                            st = origin + ((section.start - f) * msu);
                            sh = (section.end - section.start) * msu;

                        }

                        clipped.push({
                            type: 'rect',
                            mode: 3,
                            fillColor: findColor(section.color),
                            fillOpacity: 0.8,
                            clip: true,
                            radius: Math.min(meanlineh / 3, 5),
                            lineWidth: 0.3,
                            points: [
                                sl, st,
                                sw, sh
                            ]
                        });


                        clipped.push({
                            fillColor: '#000000',
                            fillOpacity: 1,
                            fontSize: tfsize,
                        });

                        let rest = 0;

                        let drawDates = false;
                        let content = "";

                        if (typeof section.label === 'string') {
                            content = section.label;
                        } else {
                            content = section.label.join('\n');
                        }

                        rest = sh - (tfsize + 1);
                        drawDates = rest >= (2 * hfsize * 0.9) && sw > 7;

                        clipped.push({
                            type: 'text',
                            x: (mode === 0) ? Math.max(origin, sl + 1) : sl + 1,
                            y: st + (drawDates ? hfsize : 0) + 1,
                            maxWidth: Math.max(0, sw - 2),
                            textAlign: align,
                            multiline: multiline,
                            maxHeight: sh - (drawDates ? 2 * hfsize : 0) - 2,
                            text: content
                        });


                        if (drawDates) {
                            clipped.push({
                                type: 'text',
                                fontSize: hfsize,
                                x: sl + 1,
                                y: st + 0.5,
                                textAlign: 'left',
                                text: (new Date(section.start)).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })
                            }, {
                                type: 'text',
                                x: sl + sw - 1,
                                y: st + sh - (hfsize * 0.9),
                                textAlign: 'right',
                                text: (new Date(section.end)).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })
                            });
                        }

                        clipped.push({
                            textAlign: 'left'
                        });

                        clipped.push({
                            type: 'restore'
                        });
                    }
                });

            });

            if (hidx === 0) {
                let title = config.title


                if (infoidx === 0) {
                    data.push({
                        type: 'text',
                        fontSize: 6,
                        fontWeight: 'bold',
                        x: marginleft,
                        y: margintop,
                        text: title
                    }, {
                        fontWeight: ''
                    });
                }
                data.push({
                    type: 'line', // activities
                    lineWidth: 0.1,
                    points: mode === 0 ?
                        [marginleft + COL_WIDTH, info.base, marginleft + COL_WIDTH, fullpageh - marginbottom] :
                        [marginleft + AXIS_WIDTH, margintop + TITLE_HEIGHT + COL_HEIGHT, fullpagew - marginright, margintop + TITLE_HEIGHT + COL_HEIGHT] // activities
                }, {
                    type: 'line', // start
                    points: mode === 0 ?
                        [marginleft, info.base, marginleft, fullpageh - marginbottom] :
                        [marginleft + AXIS_WIDTH, margintop + TITLE_HEIGHT, fullpagew - marginright, margintop + TITLE_HEIGHT] // top

                }
                );
            }

            data.push({ lineWidth: 5 }, {
                type: 'line', // bottom
                lineWidth: 0.1,
                strikeColor: '#000',
                points: mode === 0 ?
                    [marginleft + (mode * AXIS_WIDTH), fullpageh - marginbottom, fullpagew - marginright, fullpageh - marginbottom] :
                    [fullpagew - marginright, margintop + (TITLE_HEIGHT * (hidx === 0)), fullpagew - marginright, fullpageh - marginbottom] // right

            },
                {
                    type: 'line', // top
                    points: mode === 0 ? [marginleft, info.base, fullpagew - marginright, info.base] :
                        [marginleft + AXIS_WIDTH, margintop + (TITLE_HEIGHT * (hidx === 0)), marginleft + AXIS_WIDTH, fullpageh - marginbottom] // left
                });

            if (hidx === widths.length - 1) { // end
                data.push({
                    type: 'line',
                    lineWidth: 0.1,
                    strikeColor: '#000',
                    points: mode === 0 ?
                        [fullpagew - marginright, info.base, fullpagew - marginright, fullpageh - marginbottom] :
                        [marginleft + (mode * AXIS_WIDTH), fullpageh - marginbottom, fullpagew - marginright, fullpageh - marginbottom] // bottom
                });
            }

            data.push({
                fillColor: '#000',
                fillOpacity: 1
            });

            axis.forEach(function (prep) {
                data.push(...mode === 0 ?
                    drawGrid(0, prep, f, t, origin, info.base, fullpageh - marginbottom) :
                    drawGrid(1, prep, f, t, origin, marginleft + AXIS_WIDTH, fullpagew - marginright)
                );
            });

            data.push({
                type: 'clip',
                points: mode === 0 ?
                    [origin, info.base, fullpagew - origin - marginright, fullpageh - marginbottom - info.base] :
                    [marginleft + AXIS_WIDTH, origin, fullpagew - marginleft - AXIS_HEIGHT - marginright, fullpageh - origin - marginbottom]
            });

            data.push(...clipped);

            data.push({
                type: 'restore'
            });

            f = t;
        });

    });

    return pages
}
