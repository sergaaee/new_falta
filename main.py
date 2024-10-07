def limit(p1, p2, x):
    if (x <= p1):
        limit = p1
    elif (x < p2):
        limit = x
    else:
        limit = p2

    return limit
