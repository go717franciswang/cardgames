var width = $(window).width()*0.95;
var height = $(window).height()*0.6;

function draw(gameState) {
    var player2nickname = {};
    for (var i = 0; i < gameState['users'].length; i++) {
        var user = gameState['users'][i];
        player2nickname[user['player']] = user['nickname'];
    }

    var seatCount = gameState['seats'].length;

    var svg = d3.select('#container').selectAll('#game').data([gameState]);
    var svgEnter = svg.enter().append('svg').attr('id', 'game').attr('width', width).attr('height', height); 
    svgEnter.append('text').attr('class', 'cc').attr('x', width/2).attr('y', height*0.55).attr('text-anchor', 'middle');
    svgEnter.append('text').attr('class', 'pots').attr('x', width/2).attr('y', height*0.45).attr('text-anchor', 'middle');

    var ccard = svg.select('text.cc').selectAll('.card').data(function(d) {
        return d['community_cards'];
    });
    handle_card(ccard);

    svg.select('text.pots').text(function(d) {
        return 'Pots: ' + d['pots'].map(function(p) { return p['money'] }).join(' ');
    });

    var seat = svg.selectAll('g.d3seat').data(function(d) { 
        return d['seats'] 
    });

    var seatEnter = seat.enter().append('g').attr('class', 'd3seat').attr('transform', function(seat) {
        var degree = 2*Math.PI/seatCount*(seat['position']-1);
        var x = Math.cos(degree)*width*0.35+width/2;
        var y = Math.sin(degree)*height*0.35+height/2;
        return 'translate('+x+','+y+')';
    });
    seatEnter.append('text').attr('class', 'nickname').attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'money').attr('y', height*0.05).attr('dx', -width*0.05).attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'bet').attr('y', height*0.05).attr('dx', width*0.05).attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'cards').attr('y', height*0.1).attr('text-anchor', 'middle');
    seatEnter.append('rect').attr('class', 'timer')
        .attr('y', height*0.13).attr('transform', 'translate(-'+width*0.07+')').attr('width', 0).attr('height', 3);

    seat.attr('fill-opacity', function(seat) {
        if (seat['cards'] == undefined || seat['cards'].length==0) return '0.4';
        else return '1';
    });

    seat.select('.nickname').text(function(d) { 
        var dealer = d['position'] == gameState['dealer_button_pos'] ? "\uD83D\uDC81" : '';
        return d['player'] ? dealer + ' ' + player2nickname[d['player']] : '-';
    });
    seat.select('.money').text(function(d) { 
        return d['player'] ? d['money'] : '';
    });
    seat.select('.bet').text(function(d) { 
        return d['player'] ? Math.min(d['money'],d['bet']) : '';
    });
    var card = seat.select('.cards').selectAll('.card').data(function(d) { 
        return d['cards'] ? d['cards'] : [] 
    });
    handle_card(card);
    seat.select('.timer').attr('player', function(d) {
        return d['player'] ? d['player'] : 'undefined';
    });
}

function handle_card(card) {
    var cardEnterContainer = card.enter().append('tspan').attr('class', 'card');
    cardEnterContainer.append('tspan').attr('class', 'rank');
    cardEnterContainer.append('tspan').attr('class', 'suit');
    card.select('.rank').text(function(card) {
        if (card == 'unknown') return '?';
        else return rank2str[card['rank']];
    });
    card.select('.suit').text(function(card) {
        if (card == 'unknown') return '';
        else return suit2str[card['suit']];
    }).attr('class', function(card) {
        if (card == 'unknown') return 'suit';
        else return 'suit ' + suit2class[card['suit']];
    });
    card.exit().remove();
}

function startTimer(player, timeout) {
    d3.select('.timer[player="'+player+'"]').attr('width', width*0.14)
        .transition().duration(timeout).ease('linear').attr('width', 0);
}

function stopTimer(player) {
    var timer = d3.select('.timer[player="'+player+'"]');
    // https://github.com/mbostock/d3/issues/1410#issuecomment-21347123
    timer.transition();
    d3.timer.flush();
    timer.attr('width', 0);
}
