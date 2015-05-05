/**
 * Provides handler for clicking follow button(s).
 */

// Return the ajax object given a data type.
var getAjaxObj = function(jsRoutes, type) {
    var ajaxObj;
    switch (type) {
        case 'user':
            ajaxObj = jsRoutes.api.Users;
            break;
        case 'file':
            ajaxObj = jsRoutes.api.Files;
            break;
        case 'dataset':
            ajaxObj = jsRoutes.api.Datasets;
            break;
        case 'collection':
            ajaxObj = jsRoutes.api.Collections;
            break;
        default:
            ajaxObj = undefined;
    }

    return ajaxObj;
};

// Return the url of get method for a given type.
var getUrl = function (jsRoutes, id, type) {
    var route = '';
    switch (type) {
        case 'user':
            route = jsRoutes.controllers.Profile.viewProfileUUID(id);
            break;
        case 'file':
            route = jsRoutes.controllers.Files.file(id);
            break;
        case 'dataset':
            route = jsRoutes.controllers.Datasets.dataset(id);
            break;
        case 'collection':
            route = jsRoutes.controllers.Collections.collection(id);
            break;
    }

    if (route === '') {
        return '';
    } else {
        return route.url;
    }
};

// Callback function for follow/unfollow onclick.
// Call this function with 'this' as the follow button.
var followHandler = function (jsRoutes, id, type, followCallback, unfollowCallback) {
    if (this == window) {
        return;
    }

    var $followButton = $(this);
    var ajaxObj = getAjaxObj(jsRoutes, type);
    if (ajaxObj === undefined) {
        return;
    }

    var buttonText = $(this).text().trim();
    if (buttonText === 'Follow') {
        var request = ajaxObj.follow(id).ajax({ type: 'POST' });
        request.done(function (data) {
            $followButton.text("Unfollow");
            $followButton.removeClass('btn-success');
            $followButton.addClass('btn-danger');
            if (followCallback !== undefined) {
                followCallback(data);
            }
        });

        request.fail(function () {
            console.log('Failed to follow.');
        });
    } else if (buttonText === 'Unfollow') {
        var request = ajaxObj.unfollow(id).ajax({ type: 'POST' });
        request.done(function (data) {
            $followButton.text("Follow");
            $followButton.removeClass('btn-danger');
            $followButton.addClass('btn-success');
            if (unfollowCallback !== undefined) {
                unfollowCallback();
            }
        });

        request.fail(function () {
            console.log('Failed to unfollow.');
        });
    }
};

// Takes care of showing recommendations.
var recommendationHandler = function(jsRoutes, $recPanel, $recDiv, recommendations) {
    if (!$recPanel.is(':visible')) {
        if (recommendations !== undefined && recommendations.length != 0) {
            $recDiv.empty();
            for (var i = 0; i < recommendations.length; i++) {
                var recommendation = recommendations[i];
                var id = recommendation['id'];
                var name = recommendation['name'];
                var objectType = recommendation['objectType'];

                var href = getUrl(jsRoutes, id, objectType);
                $recDiv.append(
                    '<div class="col-md-10" style="margin-top: 30px">' +
                        '<div class="panel panel-default">' +
                            '<div class="panel-heading">' +
                                '<a href="' + href + '">' + name + ' </a>' +
                            '</div>' +
                            '<div class="panel-footer">' +
                                '<h4>' +
                                    '<button class="followButton btn-xs btn-success" ' +
                                            'data-toggle="button" ' +
                                            'aria-pressed="true" ' +
                                            'autocomplete="off" ' +
                                            'objectId="' + id + '" ' +
                                            'objectType="' + objectType + '">' +
                                        'Follow' +
                                    '</button>' +
                                '</h4>' +
                            '</div>' +
                        '</div>' +
                    '</div>'
                );
            }

            $recPanel.slideToggle('slow');
        }
    }
};
