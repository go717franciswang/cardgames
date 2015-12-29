
function draw(gameState) {
    var player2nickname = {};
    for (var i = 0; i < gameState['users'].length; i++) {
        var user = gameState['users'][i];
        player2nickname[user['player']] = user['nickname'];
    }

    var width = 300;
    var height = 200;
    var seatCount = gameState['seats'].length;

    var svg = d3.select('#container').selectAll('#game').data([gameState]);
    var svgEnter = svg.enter().append('svg').attr('id', 'game').attr('width', width).attr('height', height); 
    svgEnter.append('text').attr('class', 'cc').attr('x', width/2).attr('y', height*0.55).attr('text-anchor', 'middle');
    svgEnter.append('text').attr('class', 'pots').attr('x', width/2).attr('y', height*0.45).attr('text-anchor', 'middle');

    svg.select('text.cc').text(function(d) {
        return 'CC: ' + cards2str(d['community_cards']);
    });

    svg.select('text.pots').text(function(d) {
        return 'Pots: ' + d['pots'].map(function(p) { return p['money'] }).join(' ');
    });

    var seat = svg.selectAll('g.d3seat').data(function(d) { 
        return d['seats'] 
    });

    var seatEnter = seat.enter().append('g').attr('class', 'd3seat').attr('transform', function(seat) {
        var degree = 2*Math.PI/seatCount*(seat['position']-1);
        var x = Math.cos(degree)*width*0.4+width/2;
        var y = Math.sin(degree)*height*0.4+height/2;
        return 'translate('+x+','+y+')';
    });
    seatEnter.append('text').attr('class', 'nickname').attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'money').attr('y', 15).attr('dx', -20).attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'bet').attr('y', 15).attr('dx', 20).attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'cards').attr('y', 30).attr('text-anchor', 'middle');
    seat.select('.nickname').text(function(d) { 
        return d['player'] ? player2nickname[d['player']] : '-';
    });
    seat.select('.money').text(function(d) { 
        return d['player'] ? d['money'] : '';
    });
    seat.select('.bet').text(function(d) { 
        return d['player'] ? Math.min(d['money'],d['bet']) : '';
    });
    seat.select('.cards').text(function(d) { 
        if (d['player']) console.log('cards', cards2str(d['cards']));
        return d['player'] ? cards2str(d['cards']) : '';
    });

}

var suit2str = {
    'spade': '\u2660',
    'heart': '\u2665',
    'diamond': '\u2666',
    'club': '\u2663'
};
var rank2str = {
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'nine': '9',
    'ten': 'T',
    'jack': 'J',
    'queen': 'Q',
    'king': 'K',
    'ace': 'A'
};

function card2str(card) {
    if (card == 'unknown') return '?';
    return rank2str[card['rank']] + suit2str[card['suit']];
}

function cards2str(cards) {
    return cards.map(card2str).join(' ');
}

