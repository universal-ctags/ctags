// Bug reported by reidakdumont on 2018/07/05

void copyMatrix(const ::cv::Mat source_p, LaGenMatDouble *destination_p_p)
{
    if (destination_p_p->rows() != source_p.rows || destination_p_p->cols() != source_p.cols)
        *destination_p_p = LaGenMatDouble(source_p.rows, source_p.cols);
    for (int i_l = 0; i_l < source_p.rows; ++i_l)
    {
        for (int j_l = 0; j_l < source_p.cols; ++j_l)
        {
            if (source_p.type() == CV_32F)
                (*destination_p_p)(i_l,j_l) = source_p.at<float>(i_l,j_l);
            else if (source_p.type() == CV_64F)
                (*destination_p_p)(i_l,j_l) = source_p.at<double>(i_l,j_l);
        }
    }
}

void copyMatrix(const LaGenMatDouble source_p, ::cv::Mat *destination_p_p)
{
    if (destination_p_p->rows != source_p.rows() || destination_p_p->cols != source_p.cols())
        *destination_p_p = ::cv::Mat(source_p.rows(), source_p.cols(), CV_64F);
    for (int i_l = 0; i_l < source_p.rows(); ++i_l)
    {
        for (int j_l = 0; j_l < source_p.cols(); ++j_l)
        {
            if (destination_p_p->type() == CV_32F)
                destination_p_p->at<float>(i_l,j_l) = (source_p)(i_l,j_l);
            else if (destination_p_p->type() == CV_64F)
                destination_p_p->at<double>(i_l,j_l) = (source_p)(i_l,j_l);
        }
    }
}