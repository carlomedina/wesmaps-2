# wesmaps-2
[data]: all things wesmaps (an exploration on what insights can we get from Wesleyan course pages)

### another work in progress

# Background

I have been intrigued by the kind of data one can extract from Wesleyan's course pages since my sophomore year. I built a scraper back then to extract these data. My scraper was super dank though back then. Anyway, this time around, I re-wrote my scraper and I think it is exponentially better than 1.0. Aside from just scraping the course pages, I am also interested in trying to figure out my problem back then. Is it possible to estimate the flow of people from the different academic buildings at Wesleyan at a given time? I recently found a way to estimate the origin-destination matrix based on entry-exit data when I was looking at the MRT train data in the Philippines (/carlomedina/mrt-eda). So I thought of using that approach for this particular use case. 

The other recent question that I have is the following: _could we segment Wesleyan courses based on course description alone? how well do these segments align to academic divisions and departments?_ Using topic modeling, specifically Latent Dirichlet Allocation, I tried exploring this problem.


# To do's
- Create an interactive visualization for the flow exploration
- Write a report on findings
