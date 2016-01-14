var width = $(window).width()*0.95;
var height = $(window).height()*0.6;
var seatCount;

function draw(gameState) {
    var player2nickname = {};
    for (var i = 0; i < gameState['users'].length; i++) {
        var user = gameState['users'][i];
        player2nickname[user['player']] = user['nickname'];
    }

    seatCount = gameState['seats'].length;

    var svg = d3.select('#container').selectAll('#game').data([gameState]);
    var svgEnter = svg.enter().append('svg').attr('id', 'game').attr('width', width).attr('height', height); 
    svgEnter.append('text').attr('class', 'cc').attr('x', width/2).attr('y', height*0.52).attr('text-anchor', 'middle');
    svgEnter.append('text').attr('class', 'pots').attr('x', width/2).attr('y', height*0.45).attr('text-anchor', 'middle');
    var pot = svg.selectAll('g.pot').data(function(d) { return d['pots'] });
    pot.enter().append('g');
    pot.each(function(p, i) {
        var container = d3.select(this);
        container.selectAll('*').remove();
        buildChipStacks(container, p['money']);
    });
    pot.attr('class', 'pot').attr('transform', function(_, i) {
        return 'translate('+width/2+','+height*(0.52+i/5)+')';
    });
    pot.exit().remove();

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

    var seatEnter = seat.enter().append('g').attr('class', 'd3seat')
        .attr('transform', function(s) { return get_translation(s['position'], 0.40) });
    seatEnter.append('text').attr('class', 'nickname').attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'moneybet').attr('y', height*0.05).attr('text-anchor', 'middle');
    seatEnter.append('text').attr('class', 'cards').attr('y', height*0.1).attr('text-anchor', 'middle');
    seatEnter.append('rect').attr('class', 'timer')
        .attr('y', height*0.13).attr('transform', 'translate(-'+width*0.07+')').attr('width', 0).attr('height', 3);

    seat.attr('fill-opacity', function(s) {
        if (s['cards'] == undefined || s['cards'].length==0) return '0.4';
        else return '1';
    });

    seat.select('.nickname').text(function(d) { 
        var dealer = d['position'] == gameState['dealer_button_pos'] ? "\uD83D\uDC81" : '';
        return d['player'] ? dealer + ' ' + player2nickname[d['player']] : '-';
    });
    seat.select('.moneybet').text(function(d) { 
        var money = d['player'] ? d['money'] : '';
        var bet = d['player'] ? Math.min(d['money'],d['bet']) : '';
        return money + ' ' + bet;
    });
    var card = seat.select('.cards').selectAll('.card').data(function(d) { 
        return d['cards'] ? d['cards'] : [] 
    });
    handle_card(card);
    seat.select('.timer').attr('player', function(d) {
        return d['player'] ? d['player'] : 'undefined';
    });

    var chipStack = svg.selectAll('g.chipStack').data(function(d) { return d['seats'] });
    chipStack.enter().append('g').attr('class', 'chipStack')
        .attr('transform', function(s) { return get_translation(s['position'], 0.25) });
    chipStack.each(function(s) {
        var container = d3.select(this);
        if (s['bet'] == 0) {
            container.transition().attr('transform', 'translate('+width/2+','+height/2+')')
                .each('end', function() {
                    container.selectAll('*').remove();
                    container.attr('transform', 
                                   function(s) { return get_translation(s['position'], 0.25) });
                });
        } else {
            container.selectAll('*').remove();
        }
        buildChipStacks(container, s['bet']);
    });
}

function get_translation(position, m) {
    var degree = 2*Math.PI/seatCount*(position-1);
    var x = Math.cos(degree)*width*m+width/2;
    var y = Math.sin(degree)*height*m+height/2;
    return 'translate('+x+','+y+')';
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

function buildChipStacks(container, dollar) {
    var cents = dollar * 100;
    var chipTypes = [
        {
            color: "pink", 
            style: {fill: "#ff99cc", stroke: "#ff0080", "stroke-width": 3},
            denominator: 500
        },
        {
            color: "black", 
            style: {fill: "#000000", stroke: "#000000", "stroke-width": 3},
            denominator: 50
        },
        {
            color: "red",
            style: {fill: "#ff0000", stroke: "#990000", "stroke-width": 3},
            denominator: 5
        },
        {
            color: "yellow",
            style: {fill: "#ffff00", stroke: "#999900", "stroke-width": 3},
            denominator: 0.5
        },
        {
            color: "green", 
            style: {fill: "#33cc33", stroke: "#1f7a1f", "stroke-width": 3},
            denominator: 0.05
        }
    ];
    var chipsPerStack = 5;

    var chips = [];
    for (var i = 0; i < chipTypes.length; i++) {
        var t = chipTypes[i];
        var c = Math.floor(cents / (t.denominator*100));
        if (c > 0) {
            for (var j = 0; j < c; j++) {
                chips.push({color: t.color, style: t.style});
            }
            cents -= t.denominator*c*100;
        }
    }

    chips = chips.reverse();
    var stacks = [];
    for (var i = 0; i < chips.length; i+=chipsPerStack) {
        stacks.push(chips.slice(i, i+chipsPerStack).reverse());
    }

    for (var i = 0; i < stacks.length; i++) {
        var stack = stacks[i];
        for (var j = 0; j < stack.length; j++) {
            var chip = stack[j];
            var strokeDA = ''+Math.random()*0+',2,5,2,5,2';
            container.append('ellipse').attr('rx', 10).attr('ry', 3).style(chip.style).style('z-index', -1)
                .style('stroke-dasharray', strokeDA).attr('cx', i*25).attr('cy', 40-j*4);
        }
    }
}

function animateWinning(position, money) {
    console.log('animating winning', position, money);
    var container = d3.select('#game').append('g').attr('transform', 'translate('+width/2+','+height/2+')');
    buildChipStacks(container, money);
    container.transition().attr('transform', function() { return get_translation(position, 0.4); }).remove();
}

